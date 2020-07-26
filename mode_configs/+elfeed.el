;;; ~/.doom.d/mode_configs/+elfeed.el -*- lexical-binding: t; -*-

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (elfeed-get-link-at-point))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." entry quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg entry)))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise.
See `elfeed-play-with-mpv'."
  (interactive)
  (let ((entry (elfeed-get-link-at-point))
        (patterns elfeed-mpv-patterns))
    (while (and patterns (not (string-match (car elfeed-mpv-patterns) entry)))
      (setq patterns (cdr patterns)))
    (if patterns
        (elfeed-play-with-mpv)
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (shr-browse-url)))))

(map! :map elfeed-show-mode-map
      :nm "o" #'elfeed-visit-or-play-with-mpv)

(map! :map elfeed-search-mode-map
      :nm "C-SPC" #'elfeed-search-browse-url)

(add-hook 'elfeed-search-mode-hook #'elfeed-update)
(add-hook 'eww-mode-hook #'delete-other-windows)
