;;; fzf-async-music.el --- macOS Music.app integration for fzf-async -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: multimedia, matching, fzf
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; A `fzf-async' extension to interact with OSX's Music app.
;;
;; Loaded automatically when `music' is in `fzf-async-extensions' and
;; `fzf-async-setup' has been called.  Requires macOS — uses
;; `osascript' (JXA) to dump the Music.app library and to play tracks.
;;
;; Strategy: dump the entire library once via JXA, present via
;; `fzf-sync-completing-read', play the selection by persistent ID.
;;
;; Commands:
;;   `fzf-async-music'             Flat list of all tracks
;;   `fzf-async-music-by-artist'   Tracks grouped under per-artist headers
;;   `fzf-async-music-by-genre'    Tracks grouped under per-genre headers
;;                                 (candidate prefix includes the genre, so
;;                                 typing e.g. \"rock\" narrows by genre)
;;   `fzf-async-music-playlist'    Pick a playlist and play it
;;   `fzf-async-music-refresh'     Drop the cached library and playlists
;;
;; Tracks and playlists are cached separately for the session.

;;; Code:

(require 'fzf-async)
(require 'cl-lib)

(defcustom fzf-async-music-dump-timeout 30
  "Seconds to wait for the Music.app library dump before giving up."
  :type 'number
  :group 'fzf-async)

(defconst fzf-async-music--dump-script
  "var m = Application('Music');
   var t = m.tracks;
   var ids = t.persistentID();
   var ar = t.artist();
   var al = t.album();
   var nm = t.name();
   var gn = t.genre();
   var out = [];
   for (var i = 0; i < ids.length; i++) {
     out.push(ids[i] + '\\t' + ar[i] + '\\t' + al[i] + '\\t' + nm[i] + '\\t' + gn[i]);
   }
   out.join('\\n');"
  "JXA snippet returning tab-separated id/artist/album/name/genre lines.")

(defvar fzf-async-music--cache nil
  "Cached tracks, each entry a plist with `:id', `:artist', `:album',
`:name', and `:genre' keys.")

(defvar fzf-async-music--playlists-cache nil
  "Cached playlists, each entry a plist with `:id' and `:name' keys.")

(defconst fzf-async-music--playlists-script
  "var m = Application('Music');
   var p = m.playlists;
   var ids = p.persistentID();
   var names = p.name();
   var out = [];
   for (var i = 0; i < ids.length; i++) {
     out.push(ids[i] + '\\t' + names[i]);
   }
   out.join('\\n');"
  "JXA snippet returning tab-separated id/name lines for each playlist.")

(defvar fzf-async-music--items nil
  "Dynamic per-call hash table mapping candidate string -> track plist.
Bound by `fzf-async-music--read' so the `:group' callback can look up
metadata for the candidate currently being rendered.")

(defun fzf-async-music--osascript-lines (script)
  "Run JXA SCRIPT via `osascript', return non-empty stdout lines."
  (unless (eq system-type 'darwin)
    (user-error "fzf-async-music requires macOS"))
  (with-temp-buffer
    (let ((rc (with-timeout (fzf-async-music-dump-timeout
                             (user-error "Music.app query timed out after %ss"
                                         fzf-async-music-dump-timeout))
                (call-process "osascript" nil t nil
                              "-l" "JavaScript" "-e" script))))
      (unless (zerop rc)
        (user-error "osascript failed (exit %s): %s" rc (buffer-string)))
      (split-string (buffer-string) "\n" t))))

(defun fzf-async-music--dump ()
  "Dump Music.app's track library into a list of plists."
  (cl-loop for line in (fzf-async-music--osascript-lines
                        fzf-async-music--dump-script)
           for parts = (split-string line "\t")
           when (>= (length parts) 4)
           collect (list :id     (nth 0 parts)
                         :artist (nth 1 parts)
                         :album  (nth 2 parts)
                         :name   (nth 3 parts)
                         :genre  (or (nth 4 parts) ""))))

(defun fzf-async-music--dump-playlists ()
  "Dump Music.app's playlists into a list of plists."
  (cl-loop for line in (fzf-async-music--osascript-lines
                        fzf-async-music--playlists-script)
           for parts = (split-string line "\t")
           when (= (length parts) 2)
           collect (list :id (nth 0 parts) :name (nth 1 parts))))

(defun fzf-async-music--tracks ()
  "Return cached track list, dumping Music.app on first use."
  (or fzf-async-music--cache
      (setq fzf-async-music--cache
            (with-temp-message "Loading Music.app library..."
              (fzf-async-music--dump)))))

(defun fzf-async-music--playlists ()
  "Return cached playlist list, dumping Music.app on first use."
  (or fzf-async-music--playlists-cache
      (setq fzf-async-music--playlists-cache
            (with-temp-message "Loading Music.app playlists..."
              (fzf-async-music--dump-playlists)))))

(defun fzf-async-music--read (tracks group-key prompt)
  "Read TRACKS via `fzf-sync-completing-read'; return the chosen plist.
GROUP-KEY is one of nil, `:artist', or `:genre'.  When non-nil:
- TRACKS are sorted by GROUP-KEY so consecutive same-key entries cluster.
- For `:genre' the group value is also prefixed to the candidate string
  so fuzzy queries can match on it.
- A `:group' function is installed for vertico-style section headers and
  strips the redundant prefix in TRANSFORM=t mode."
  (let* ((sorted (if group-key
                     (cl-sort (copy-sequence tracks) #'string<
                              :key (lambda (p)
                                     (downcase (or (plist-get p group-key) ""))))
                   tracks))
         (map (make-hash-table :test #'equal))
         (cands
          (mapcar
           (lambda (p)
             (let* ((base (format "%s — %s — %s"
                                  (plist-get p :artist)
                                  (plist-get p :album)
                                  (plist-get p :name)))
                    (cand (if (eq group-key :genre)
                              (format "%s — %s"
                                      (or (plist-get p :genre) "(no genre)")
                                      base)
                            base)))
               (puthash cand p map)
               cand))
           sorted))
         (fzf-async-music--items map)
         (group-fn
          (when group-key
            (lambda (cand transform)
              (let ((p (gethash cand fzf-async-music--items)))
                (cond
                 ((null p) cand)
                 ((null transform)
                  (let ((g (plist-get p group-key)))
                    (if (or (null g) (string-empty-p g)) "(none)" g)))
                 ;; TRANSFORM=t — strip the redundant prefix from the
                 ;; per-row display so we don't double-show the group key.
                 ((eq group-key :genre)
                  (format "%s — %s — %s"
                          (plist-get p :artist) (plist-get p :album)
                          (plist-get p :name)))
                 ((eq group-key :artist)
                  (format "%s — %s"
                          (plist-get p :album) (plist-get p :name)))
                 (t cand)))))))
    (when-let* ((sel (fzf-sync-completing-read
                      :candidates cands
                      :prompt prompt
                      :category 'fzf-async-music
                      :group group-fn)))
      (gethash sel fzf-async-music--items))))

(defun fzf-async-music--pick-and-play (group-key prompt)
  "Pick a track grouped by GROUP-KEY (or flat) with PROMPT, then play it."
  (when-let* ((item (fzf-async-music--read
                     (fzf-async-music--tracks) group-key prompt)))
    (call-process
     "osascript" nil 0 nil "-e"
     (format
      "tell application \"Music\" to play (some track whose persistent ID is %S)"
      (plist-get item :id)))))

;;;###autoload
(defun fzf-async-music-refresh ()
  "Invalidate cached Music.app library and playlists so they re-dump."
  (interactive)
  (setq fzf-async-music--cache           nil
        fzf-async-music--playlists-cache nil)
  (message "Music.app caches cleared"))

(defun fzf-async-music--pick-playlist (prompt)
  "Fuzzy-select a Music.app playlist with PROMPT; return its plist or nil."
  (let* ((playlists (fzf-async-music--playlists))
         (map (make-hash-table :test #'equal))
         (cands (mapcar (lambda (p)
                          (let ((n (plist-get p :name)))
                            (puthash n p map) n))
                        playlists)))
    (when-let* ((sel (fzf-sync-completing-read
                      :candidates cands
                      :prompt prompt
                      :category 'fzf-async-music)))
      (gethash sel map))))

(defun fzf-async-music--play-playlist (item shuffle)
  "Play playlist ITEM with shuffle on or off per SHUFFLE."
  (call-process
   "osascript" nil 0 nil "-e"
   (format
    "tell application \"Music\"\nset shuffle enabled to %s\nplay (first playlist whose persistent ID is %S)\nend tell"
    (if shuffle "true" "false")
    (plist-get item :id))))

;;;###autoload
(defun fzf-async-music-playlist ()
  "Fuzzy-select a Music.app playlist and play it sequentially.
Explicitly disables shuffle so this command always plays in order,
even if `fzf-async-music-playlist-shuffle' was used previously."
  (interactive)
  (when-let* ((item (fzf-async-music--pick-playlist "playlist: ")))
    (fzf-async-music--play-playlist item nil)))

;;;###autoload
(defun fzf-async-music-playlist-shuffle ()
  "Fuzzy-select a Music.app playlist and play it in shuffle mode."
  (interactive)
  (when-let* ((item (fzf-async-music--pick-playlist "playlist (shuffle): ")))
    (fzf-async-music--play-playlist item t)))

;;;###autoload
(defun fzf-async-music ()
  "Fuzzy-select and play a track from the macOS Music.app library."
  (interactive)
  (fzf-async-music--pick-and-play nil "music: "))

;;;###autoload
(defun fzf-async-music-by-artist ()
  "Fuzzy-select and play a track, with results grouped by artist."
  (interactive)
  (fzf-async-music--pick-and-play :artist "music (by artist): "))

;;;###autoload
(defun fzf-async-music-by-genre ()
  "Fuzzy-select and play a track, with results grouped by genre.
Genre is prefixed to each candidate, so typing the genre narrows results."
  (interactive)
  (fzf-async-music--pick-and-play :genre "music (by genre): "))

;;;###autoload
(defun fzf-async-music-setup ()
  "Register the `fzf-async-music' completion category."
  (add-to-list 'completion-category-overrides
               '(fzf-async-music (styles fzf-async))))

(provide 'fzf-async-music)
;;; fzf-async-music.el ends here
