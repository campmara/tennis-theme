; ==================================================================================================
; T H E M E :  N O R A
; This is a custom theme for emacs based on personal preferences.
; ==================================================================================================

(deftheme nora
  "Nora - A custom emacs theme for Mara based on her personal preferences.")

(defvar nora-colors-alist
  (let ((colors `(("nora-accent"   . "#a7c080")
		  ("nora-fg"       . "#c4d3aa")
		  ("nora-bg"       . "#173030")
		  ("nora-bg-1"     . "#325252")
		  ("nora-bg-hl"    . "#3a454a")
		  ("nora-gutter"   . "#142424")
		  ("nora-mono-1"   . "#abb2bf")
		  ("nora-mono-2"   . "#503946")
		  ("nora-mono-3"   . "#47c987")
		  ("nora-cyan"     . "#83c092")
		  ("nora-blue"     . "#7fbbb3")
		  ("nora-purple"   . "#d699b6")
		  ("nora-green"    . "#a7c080")
		  ("nora-red"      . "#e67e80")
		  ("nora-orange"   . "#e69875")
		  ("nora-yellow"   . "#ddbc7f")
		  ("nora-gray"     . "#325252")
		  ("nora-silver"   . "#9da9a0")
		  ("nora-black"    . "#173030")
		  ("nora-border"   . "#142424")
		  ("nora-visual"   . "#325252"))))
    colors)
  "List of nora Colors.")

(defmacro nora-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
	 ,@ (mapcar (lambda (cons)
		      (list (intern (car cons)) (cdr cons)))
		    nora-colors-alist))
     ,@body))

(nora-with-color-variables
  (custom-theme-set-faces
   'nora

   `(default             ((t (:foreground ,nora-fg :background ,nora-bg))))
   `(success             ((t (:foreground ,nora-green))))
   `(warning             ((t (:foreground ,nora-yellow))))
   `(error               ((t (:foreground ,nora-red :weight bold))))
   `(link                ((t (:foreground ,nora-blue :underline t :weight bold))))
   `(link-visited        ((t (:foreground ,nora-blue :underline t :weight normal))))
   `(cursor              ((t (:background ,nora-fg))))
   `(fringe              ((t (:background ,nora-bg :foreground ,nora-silver))))
   `(region              ((t (:background ,nora-gutter :distant-foreground ,nora-mono-2))))
   `(highlight           ((t (:background ,nora-gray :distant-foreground ,nora-mono-2))))
   `(hl-line             ((t (:background ,nora-visual :distant-foreground nil))))
   `(header-line         ((t (:background ,nora-black))))
   `(vertical-border     ((t (:background ,nora-border :foreground ,nora-border))))
   `(secondary-selection ((t (:background ,nora-bg-1))))
   `(query-replace       ((t (:inherit (isearch)))))
   `(minibuffer-prompt   ((t (:foreground ,nora-blue))))
   `(tooltip             ((t (:foreground ,nora-fg :background ,nora-bg-1 :inherit variable-pitch))))

   `(font-lock-builtin-face           ((t (:foreground ,nora-cyan))))
   `(font-lock-comment-face           ((t (:foreground ,nora-mono-3 :slant italic))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face               ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face     ((t (:foreground ,nora-green))))
   `(font-lock-keyword-face           ((t (:foreground ,nora-red :weight normal))))
   `(font-lock-preprocessor-face      ((t (:foreground ,nora-red))))
   `(font-lock-string-face            ((t (:foreground ,nora-green))))
   `(font-lock-type-face              ((t (:foreground ,nora-green))))
   `(font-lock-constant-face          ((t (:foreground ,nora-cyan))))
   `(font-lock-variable-name-face     ((t (:foreground ,nora-blue))))
   `(font-lock-warning-face           ((t (:foreground ,nora-mono-3 :bold t))))
   `(font-lock-negation-char-face     ((t (:foreground ,nora-cyan :bold t))))
   `(highlight-numbers-number         ((t (:foreground ,nora-purple))))

   ;; eob
   `(vi-tilde-fringe-face ((t (:foreground ,nora-silver))))
   `(solaire-fringe-face  ((t (:foreground ,nora-silver))))

   ;; mode-line
   `(mode-line           ((t (:background ,nora-visual :foreground ,nora-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis  ((t (:weight bold))))
   `(mode-line-inactive  ((t (:background ,nora-gutter :foreground ,nora-silver))))

   ;; window-divider
   `(window-divider             ((t (:foreground ,nora-border))))
   `(window-divider-first-pixel ((t (:foreground ,nora-border))))
   `(window-divider-last-pixel  ((t (:foreground ,nora-border))))

   ;; custom
   `(custom-state ((t (:foreground ,nora-green))))

   ;; Package/Plugin Customizations

   ;; ido
   `(ido-first-match ((t (:foreground ,nora-purple :weight bold))))
   `(ido-only-match  ((t (:foreground ,nora-red :weight bold))))
   `(ido-subdir      ((t (:foreground ,nora-blue))))
   `(ido-virtual     ((t (:foreground ,nora-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,nora-mono-3 :background ,nora-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,nora-red :background ,nora-bg-1 :inverse-video nil))))

   ;; ace-window
   `(aw-background-face   ((t (:inherit font-lock-comment-face))))
   `(aw-leading-char-face ((t (:foreground ,nora-red :weight bold))))

   ;; centaur-tabs
   `(centaur-tabs-default           ((t (:background ,nora-black :foreground ,nora-black))))
   `(centaur-tabs-selected          ((t (:background ,nora-bg :foreground ,nora-fg :weight bold))))
   `(centaur-tabs-unselected        ((t (:background ,nora-black :foreground ,nora-fg :weight light))))
   `(centaur-tabs-selected-modified ((t (:background ,nora-bg
					 :foreground ,nora-blue :weight bold))))
   `(centaur-tabs-unselected-modified ((t (:background ,nora-black :weight light
					   :foreground ,nora-blue))))
   `(centaur-tabs-active-bar-face            ((t (:background ,nora-accent))))
   `(centaur-tabs-modified-marker-selected   ((t (:inherit 'centaur-tabs-selected :foreground,nora-accent))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected :foreground,nora-accent))))

   ;; company-mode
   `(company-tooltip                      ((t (:foreground ,nora-fg :background ,nora-gray))))
   `(company-tooltip-annotation           ((t (:foreground ,nora-mono-2 :background ,nora-gray))))
   `(company-tooltip-annotation-selection ((t (:foreground ,nora-mono-2 :background ,nora-mono-3))))
   `(company-tooltip-selection            ((t (:foreground ,nora-fg :background ,nora-mono-3))))
   `(company-tooltip-mouse                ((t (:background ,nora-gray))))
   `(company-tooltip-common               ((t (:foreground ,nora-yellow :background ,nora-gray))))
   `(company-tooltip-common-selection     ((t (:foreground ,nora-yellow :background ,nora-mono-3))))
   `(company-preview                      ((t (:background ,nora-bg))))
   `(company-preview-common               ((t (:foreground ,nora-yellow :background ,nora-bg))))
   `(company-scrollbar-fg                 ((t (:background ,nora-mono-1))))
   `(company-scrollbar-bg                 ((t (:background ,nora-gray))))
   `(company-template-field               ((t (:inherit highlight))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,nora-accent))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,nora-cyan :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,nora-red :style wave)))))

   ;; flymake
   `(flymake-error   ((t (:underline (:color ,nora-red :style wave)))))
   `(flymake-note    ((t (:underline (:color ,nora-green :style wave)))))
   `(flymake-warning ((t (:underline (:color ,nora-orange :style wave)))))

   ;; flycheck
   `(flycheck-error   ((t (:underline (:color ,nora-red :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,nora-green :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,nora-orange :style wave)))))

   ;; compilation
   `(compilation-face           ((t (:foreground ,nora-fg))))
   `(compilation-line-number    ((t (:foreground ,nora-mono-2))))
   `(compilation-column-number  ((t (:foreground ,nora-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch        ((t (:foreground ,nora-bg :background ,nora-purple))))
   `(isearch-fail   ((t (:foreground ,nora-red :background nil))))
   `(lazy-highlight ((t (:foreground ,nora-purple :background ,nora-bg-1 :underline ,nora-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged       ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink       ((t (:foreground "#FD5FF1"))))
   `(diredfl-file-name   ((t (:foreground ,nora-fg))))
   `(diredfl-file-suffix ((t (:foreground ,nora-fg))))
   `(diredfl-number      ((t (:foreground ,nora-red))))
   `(diredfl-date-time   ((t (:foreground ,nora-blue))))
   `(diredfl-no-priv     ((t (:foreground ,nora-yellow))))
   `(diredfl-dir-priv    ((t (:foreground ,nora-yellow))))
   `(diredfl-read-priv   ((t (:foreground ,nora-yellow))))
   `(diredfl-write-priv  ((t (:foreground ,nora-yellow))))
   `(diredfl-exec-priv   ((t (:foreground ,nora-yellow))))
   `(diredfl-link-priv   ((t (:foreground ,nora-yellow))))
   `(diredfl-dir-heading ((t (:foreground ,nora-green :weight bold))))

   ;; dired-async
   `(dired-async-failures     ((t (:inherit error))))
   `(dired-async-message      ((t (:inherit success))))
   `(dired-async-mode-message ((t (:foreground ,nora-orange))))

   ;; helm
   `(helm-header ((t (:foreground ,nora-mono-2
		      :background ,nora-bg
		      :underline nil
		      :box (:line-width 6 :color ,nora-bg)))))
   `(helm-source-header ((t (:foreground ,nora-yellow
			     :background ,nora-bg
			     :underline nil
			     :weight bold
			     :box (:line-width 6 :color ,nora-bg)))))
   `(helm-selection                    ((t (:background ,nora-gray))))
   `(helm-selection-line               ((t (:background ,nora-gray))))
   `(helm-visible-mark                 ((t (:background ,nora-bg :foreground ,nora-yellow))))
   `(helm-candidate-number             ((t (:foreground ,nora-green :background ,nora-bg-1))))
   `(helm-separator                    ((t (:background ,nora-bg :foreground ,nora-red))))
   `(helm-M-x-key                      ((t (:foreground ,nora-orange))))
   `(helm-bookmark-addressbook         ((t (:foreground ,nora-orange))))
   `(helm-bookmark-directory           ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file                ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus                ((t (:foreground ,nora-purple))))
   `(helm-bookmark-info                ((t (:foreground ,nora-green))))
   `(helm-bookmark-man                 ((t (:foreground ,nora-yellow))))
   `(helm-bookmark-w3m                 ((t (:foreground ,nora-purple))))
   `(helm-match                        ((t (:foreground ,nora-yellow))))
   `(helm-ff-directory                 ((t (:foreground ,nora-cyan :background ,nora-bg :weight bold))))
   `(helm-ff-file                      ((t (:foreground ,nora-fg :background ,nora-bg :weight normal))))
   `(helm-ff-executable                ((t (:foreground ,nora-green :background ,nora-bg :weight normal))))
   `(helm-ff-invalid-symlink           ((t (:foreground ,nora-red :background ,nora-bg :weight bold))))
   `(helm-ff-symlink                   ((t (:foreground ,nora-yellow :background ,nora-bg :weight bold))))
   `(helm-ff-prefix                    ((t (:foreground ,nora-bg :background ,nora-yellow :weight normal))))
   `(helm-buffer-not-saved             ((t (:foreground ,nora-red))))
   `(helm-buffer-process               ((t (:foreground ,nora-mono-2))))
   `(helm-buffer-saved-out             ((t (:foreground ,nora-fg))))
   `(helm-buffer-size                  ((t (:foreground ,nora-mono-2))))
   `(helm-buffer-directory             ((t (:foreground ,nora-purple))))
   `(helm-grep-cmd-line                ((t (:foreground ,nora-cyan))))
   `(helm-grep-file                    ((t (:foreground ,nora-fg))))
   `(helm-grep-finish                  ((t (:foreground ,nora-green))))
   `(helm-grep-lineno                  ((t (:foreground ,nora-mono-2))))
   `(helm-grep-finish                  ((t (:foreground ,nora-red))))
   `(helm-grep-match                   ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,nora-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face       ((t (:background ,nora-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face       ((t (:background ,nora-purple :foreground "#ffffff"))))
   `(helm-locate-finish                ((t (:foreground ,nora-green))))
   `(info-menu-star                    ((t (:foreground ,nora-red))))

   ;; ivy
   `(ivy-confirm-face               ((t (:inherit minibuffer-prompt :foreground ,nora-green))))
   `(ivy-current-match              ((t (:background ,nora-gray :weight normal))))
   `(ivy-highlight-face             ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face        ((t (:inherit minibuffer-prompt :foreground ,nora-red))))
   `(ivy-minibuffer-match-face-1    ((t (:background ,nora-bg-hl))))
   `(ivy-minibuffer-match-face-2    ((t (:inherit ivy-minibuffer-match-face-1 :background ,nora-black :foreground ,nora-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3    ((t (:inherit ivy-minibuffer-match-face-2 :background ,nora-black :foreground ,nora-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4    ((t (:inherit ivy-minibuffer-match-face-2 :background ,nora-black :foreground ,nora-yellow :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:inherit ivy-current-match))))
   `(ivy-modified-buffer            ((t (:inherit default :foreground ,nora-orange))))
   `(ivy-virtual                    ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,nora-yellow :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,nora-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,nora-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,nora-yellow :weight bold))))

   ;; git-gutter
   `(git-gutter:added    ((t (:foreground ,nora-green :weight bold))))
   `(git-gutter:deleted  ((t (:foreground ,nora-red :weight bold))))
   `(git-gutter:modified ((t (:foreground ,nora-orange :weight bold))))

   ;; eshell
   `(eshell-ls-archive    ((t (:foreground ,nora-purple :weight bold))))
   `(eshell-ls-backup     ((t (:foreground ,nora-yellow))))
   `(eshell-ls-clutter    ((t (:foreground ,nora-red :weight bold))))
   `(eshell-ls-directory  ((t (:foreground ,nora-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,nora-green :weight bold))))
   `(eshell-ls-missing    ((t (:foreground ,nora-red :weight bold))))
   `(eshell-ls-product    ((t (:foreground ,nora-yellow))))
   `(eshell-ls-special    ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink    ((t (:foreground ,nora-cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,nora-mono-1))))
   `(eshell-prompt        ((t (:inherit minibuffer-prompt))))

   ;; man
   `(Man-overstrike ((t (:foreground ,nora-green :weight bold))))
   `(Man-underline  ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold   ((t (:foreground ,nora-green :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face     ((t (:inherit widget-button))))
   `(dictionary-reference-face  ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; erc
   `(erc-error-face     ((t (:inherit error))))
   `(erc-input-face     ((t (:inherit shadow))))
   `(erc-my-nick-face   ((t (:foreground ,nora-accent))))
   `(erc-notice-face    ((t (:inherit font-lock-comment-face))))
   `(erc-timestamp-face ((t (:foreground ,nora-green :weight bold))))

   ;; jabber
   `(jabber-roster-user-online     ((t (:foreground ,nora-green))))
   `(jabber-roster-user-away       ((t (:foreground ,nora-red))))
   `(jabber-roster-user-xa         ((t (:foreground ,nora-red))))
   `(jabber-roster-user-dnd        ((t (:foreground ,nora-purple))))
   `(jabber-roster-user-chatty     ((t (:foreground ,nora-yellow))))
   `(jabber-roster-user-error      ((t (:foreground ,nora-red :bold t))))
   `(jabber-roster-user-offline    ((t (:foreground ,nora-mono-3))))
   `(jabber-chat-prompt-local      ((t (:foreground ,nora-blue))))
   `(jabber-chat-prompt-foreign    ((t (:foreground ,nora-yellow))))
   `(jabber-chat-prompt-system     ((t (:foreground ,nora-mono-3))))
   `(jabber-chat-error             ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face         ((t (:foreground ,nora-cyan))))
   `(jabber-activity-face          ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; eww
   `(eww-form-checkbox       ((t (:inherit eww-form-submit))))
   `(eww-form-file           ((t (:inherit eww-form-submit))))
   `(eww-form-select         ((t (:inherit eww-form-submit))))
   `(eww-form-submit         ((t (:background ,nora-gray :foreground ,nora-fg :box (:line-width 2 :color ,nora-border :style released-button)))))
   `(eww-form-text           ((t (:inherit widget-field :box (:line-width 1 :color ,nora-border)))))
   `(eww-form-textarea       ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,nora-red))))
   `(eww-valid-certificate   ((t (:foreground ,nora-green))))

   ;; ediff
   `(ediff-fine-diff-Ancestor      ((t (:background "#885555"))))
   `(ediff-fine-diff-A             ((t (:background "#885555"))))
   `(ediff-fine-diff-B             ((t (:background "#558855"))))
   `(ediff-fine-diff-C             ((t (:background "#555588"))))
   `(ediff-current-diff-Ancestor   ((t (:background "#663333"))))
   `(ediff-current-diff-A          ((t (:background "#663333"))))
   `(ediff-current-diff-B          ((t (:background "#336633"))))
   `(ediff-current-diff-C          ((t (:background "#333366"))))
   `(ediff-even-diff-Ancestor      ((t (:background "#181a1f"))))
   `(ediff-even-diff-A             ((t (:background "#181a1f"))))
   `(ediff-even-diff-B             ((t (:background "#181a1f"))))
   `(ediff-even-diff-C             ((t (:background "#181a1f"))))
   `(ediff-odd-diff-Ancestor       ((t (:background "#181a1f"))))
   `(ediff-odd-diff-A              ((t (:background "#181a1f"))))
   `(ediff-odd-diff-B              ((t (:background "#181a1f"))))
   `(ediff-odd-diff-C              ((t (:background "#181a1f"))))

   ;; magit
   `(magit-section-highlight           ((t (:background ,nora-bg-hl))))
   `(magit-section-heading             ((t (:foreground ,nora-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,nora-fg :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,nora-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,nora-yellow :background ,nora-bg-hl :weight bold))))
   `(magit-diff-hunk-heading           ((t (:foreground ,nora-mono-2 :background ,nora-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,nora-mono-1 :background ,nora-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,nora-purple :background ,nora-mono-3))))
   `(magit-diff-context                ((t (:foreground ,nora-fg))))
   `(magit-diff-context-highlight      ((t (:background ,nora-bg-1 :foreground ,nora-fg))))
   `(magit-diffstat-added              ((t (:foreground ,nora-green))))
   `(magit-diffstat-removed            ((t (:foreground ,nora-red))))
   `(magit-process-ok                  ((t (:foreground ,nora-green))))
   `(magit-process-ng                  ((t (:foreground ,nora-red))))
   `(magit-log-author                  ((t (:foreground ,nora-yellow))))
   `(magit-log-date                    ((t (:foreground ,nora-mono-2))))
   `(magit-log-graph                   ((t (:foreground ,nora-silver))))
   `(magit-sequence-pick               ((t (:foreground ,nora-yellow))))
   `(magit-sequence-stop               ((t (:foreground ,nora-green))))
   `(magit-sequence-part               ((t (:foreground ,nora-orange))))
   `(magit-sequence-head               ((t (:foreground ,nora-blue))))
   `(magit-sequence-drop               ((t (:foreground ,nora-red))))
   `(magit-sequence-done               ((t (:foreground ,nora-mono-2))))
   `(magit-sequence-onto               ((t (:foreground ,nora-mono-2))))
   `(magit-bisect-good                 ((t (:foreground ,nora-green))))
   `(magit-bisect-skip                 ((t (:foreground ,nora-orange))))
   `(magit-bisect-bad                  ((t (:foreground ,nora-red))))
   `(magit-blame-heading               ((t (:background ,nora-bg-1 :foreground ,nora-mono-2))))
   `(magit-blame-hash                  ((t (:background ,nora-bg-1 :foreground ,nora-purple))))
   `(magit-blame-name                  ((t (:background ,nora-bg-1 :foreground ,nora-yellow))))
   `(magit-blame-date                  ((t (:background ,nora-bg-1 :foreground ,nora-mono-3))))
   `(magit-blame-summary               ((t (:background ,nora-bg-1 :foreground ,nora-mono-2))))
   `(magit-dimmed                      ((t (:foreground ,nora-mono-2))))
   `(magit-hash                        ((t (:foreground ,nora-purple))))
   `(magit-tag                         ((t (:foreground ,nora-orange :weight bold))))
   `(magit-branch-remote               ((t (:foreground ,nora-green :weight bold))))
   `(magit-branch-local                ((t (:foreground ,nora-blue :weight bold))))
   `(magit-branch-current              ((t (:foreground ,nora-blue :weight bold :box t))))
   `(magit-head                        ((t (:foreground ,nora-blue :weight bold))))
   `(magit-refname                     ((t (:background ,nora-bg :foreground ,nora-fg :weight bold))))
   `(magit-refname-stash               ((t (:background ,nora-bg :foreground ,nora-fg :weight bold))))
   `(magit-refname-wip                 ((t (:background ,nora-bg :foreground ,nora-fg :weight bold))))
   `(magit-signature-good              ((t (:foreground ,nora-green))))
   `(magit-signature-bad               ((t (:foreground ,nora-red))))
   `(magit-signature-untrusted         ((t (:foreground ,nora-orange))))
   `(magit-cherry-unmatched            ((t (:foreground ,nora-cyan))))
   `(magit-cherry-equivalent           ((t (:foreground ,nora-purple))))
   `(magit-reflog-commit               ((t (:foreground ,nora-green))))
   `(magit-reflog-amend                ((t (:foreground ,nora-purple))))
   `(magit-reflog-merge                ((t (:foreground ,nora-green))))
   `(magit-reflog-checkout             ((t (:foreground ,nora-blue))))
   `(magit-reflog-reset                ((t (:foreground ,nora-red))))
   `(magit-reflog-rebase               ((t (:foreground ,nora-purple))))
   `(magit-reflog-cherry-pick          ((t (:foreground ,nora-green))))
   `(magit-reflog-remote               ((t (:foreground ,nora-cyan))))
   `(magit-reflog-other                ((t (:foreground ,nora-cyan))))

   ;; message
   `(message-cited-text         ((t (:foreground ,nora-green))))
   `(message-header-cc          ((t (:foreground ,nora-orange :weight bold))))
   `(message-header-name        ((t (:foreground ,nora-purple))))
   `(message-header-newsgroups  ((t (:foreground ,nora-yellow :weight bold :slant italic))))
   `(message-header-other       ((t (:foreground ,nora-red))))
   `(message-header-subject     ((t (:foreground ,nora-blue))))
   `(message-header-to          ((t (:foreground ,nora-yellow :weight bold))))
   `(message-header-xheader     ((t (:foreground ,nora-silver))))
   `(message-mml                ((t (:foreground ,nora-purple))))
   `(message-separator          ((t (:foreground ,nora-mono-3 :slant italic))))

   ;; epa
   `(epa-field-body ((t (:foreground ,nora-blue :slant italic))))
   `(epa-field-name ((t (:foreground ,nora-cyan :weight bold))))

   ;; notmuch
   `(notmuch-crypto-decryption            ((t (:foreground ,nora-purple :background ,nora-black))))
   `(notmuch-crypto-signature-bad         ((t (:foreground ,nora-red :background ,nora-black))))
   `(notmuch-crypto-signature-good        ((t (:foreground ,nora-green :background ,nora-black))))
   `(notmuch-crypto-signature-good-key    ((t (:foreground ,nora-green :background ,nora-black))))
   `(notmuch-crypto-signature-unknown     ((t (:foreground ,nora-orange :background ,nora-black))))
   `(notmuch-hello-logo-background        ((t (:inherit default))))
   `(notmuch-message-summary-face         ((t (:background ,nora-black))))
   `(notmuch-search-count                 ((t (:inherit default :foreground ,nora-silver))))
   `(notmuch-search-date                  ((t (:inherit default :foreground ,nora-purple))))
   `(notmuch-search-matching-authors      ((t (:inherit default :foreground ,nora-yellow))))
   `(notmuch-search-non-matching-authors  ((t (:inherit font-lock-comment-face :slant italic))))
   `(notmuch-tag-added                    ((t (:underline t))))
   `(notmuch-tag-deleted                  ((t (:strike-through ,nora-red))))
   `(notmuch-tag-face                     ((t (:foreground ,nora-green))))
   `(notmuch-tag-unread                   ((t (:foreground ,nora-red))))
   `(notmuch-tree-match-author-face       ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face         ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face              ((t (:weight semi-bold))))
   `(notmuch-tree-match-tag-face          ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-face           ((t (:slant italic :weight light :inherit font-lock-comment-face))))

   ;; mu4e
   `(mu4e-header-key-face      ((t (:foreground ,nora-green :weight bold))))
   `(mu4e-header-title-face    ((t (:foreground ,nora-blue))))
   `(mu4e-title-face           ((t (:foreground ,nora-green :weight bold))))

   ;; calendar
   `(cfw:face-title                  ((t (:foreground ,nora-green :weight bold))))
   `(cfw:face-select                 ((t (:foreground ,nora-bg :background ,nora-fg))))
   `(cfw:face-header                 ((t (:foreground ,nora-fg :weight bold))))
   `(cfw:face-sunday                 ((t (:foreground ,nora-green :weight bold))))
   `(cfw:face-holiday                ((t (:foreground ,nora-blue :weight bold))))
   `(cfw:face-toolbar                ((t (:background ,nora-bg))))
   `(cfw:face-toolbar-button-on      ((t (:foreground ,nora-fg :weight bold))))
   `(cfw:face-toolbar-button-off     ((t (:foreground ,nora-silver :weight bold))))
   `(cfw:face-day-title              ((t (:background ,nora-bg))))
   `(cfw:face-today-title            ((t (:foreground ,nora-bg :background ,nora-green))))

   ;; elfeed
   `(elfeed-log-debug-level-face      ((t (:background ,nora-black :foreground ,nora-green))))
   `(elfeed-log-error-level-face      ((t (:background ,nora-black :foreground ,nora-red))))
   `(elfeed-log-info-level-face       ((t (:background ,nora-black :foreground ,nora-blue))))
   `(elfeed-log-warn-level-face       ((t (:background ,nora-black :foreground ,nora-orange))))
   `(elfeed-search-date-face          ((t (:foreground ,nora-purple))))
   `(elfeed-search-feed-face          ((t (:foreground ,nora-yellow))))
   `(elfeed-search-tag-face           ((t (:foreground ,nora-green))))
   `(elfeed-search-title-face         ((t (:foreground ,nora-silver))))
   `(elfeed-search-unread-title-face  ((t (:foreground ,nora-mono-1 :weight bold))))
   `(elfeed-search-unread-count-face  ((t (:foreground ,nora-silver))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,nora-blue))))

   ;; powerline
   `(powerline-active1    ((,class (:background ,nora-bg-hl :foreground ,nora-purple))))
   `(powerline-active2    ((,class (:background ,nora-bg-hl :foreground ,nora-purple))))
   `(powerline-inactive1  ((,class (:background ,nora-bg :foreground ,nora-fg))))
   `(powerline-inactive2  ((,class (:background ,nora-bg :foreground ,nora-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,nora-green))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,nora-red))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,nora-blue))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,nora-cyan))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,nora-purple))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,nora-yellow))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,nora-orange))))
   `(rainbow-delimiters-depth-8-face    ((t (:foreground ,nora-green))))
   `(rainbow-delimiters-depth-9-face    ((t (:foreground ,nora-orange))))
   `(rainbow-delimiters-depth-10-face   ((t (:foreground ,nora-cyan))))
   `(rainbow-delimiters-depth-11-face   ((t (:foreground ,nora-purple))))
   `(rainbow-delimiters-depth-12-face   ((t (:foreground ,nora-yellow))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,nora-red :weight bold))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,nora-green))))

   ;; elixir
   `(elixir-atom-face       ((t (:foreground ,nora-cyan))))
   `(elixir-attribute-face  ((t (:foreground ,nora-red))))

   ;; show-paren
   `(show-paren-match     ((,class (:foreground ,nora-purple :inherit bold :underline t))))
   `(show-paren-mismatch  ((,class (:foreground ,nora-red :inherit bold :underline t))))

   ;; cider
   `(cider-fringe-good-face ((t (:foreground ,nora-green))))

   ;; sly
   `(sly-error-face          ((t (:underline (:color ,nora-red :style wave)))))
   `(sly-mrepl-note-face     ((t (:inherit font-lock-comment-face))))
   `(sly-mrepl-output-face   ((t (:inherit font-lock-string-face))))
   `(sly-mrepl-prompt-face   ((t (:inherit comint-highlight-prompt))))
   `(sly-note-face           ((t (:underline (:color ,nora-green :style wave)))))
   `(sly-style-warning-face  ((t (:underline (:color ,nora-yellow :style wave)))))
   `(sly-warning-face        ((t (:underline (:color ,nora-orange :style wave)))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,nora-red :background ,nora-gray :weight bold))))
   `(sp-show-pair-match-face    ((t (:foreground ,nora-blue :weight bold :underline t))))

   ;; lispy
   `(lispy-face-hint ((t (:background ,nora-border :foreground ,nora-yellow))))

   ;; lispyville
   `(lispyville-special-face ((t (:foreground ,nora-red))))

   ;; spaceline
   `(spaceline-flycheck-error    ((,class (:foreground ,nora-red))))
   `(spaceline-flycheck-info     ((,class (:foreground ,nora-green))))
   `(spaceline-flycheck-warning  ((,class (:foreground ,nora-orange))))
   `(spaceline-python-venv       ((,class (:foreground ,nora-purple))))

   ;; solaire mode
   `(solaire-default-face      ((,class (:inherit default :background ,nora-black))))
   `(solaire-minibuffer-face   ((,class (:inherit default :background ,nora-black))))

   ;; web-mode
   `(web-mode-doctype-face            ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face              ((t (:background ,nora-black :foreground ,nora-red))))
   `(web-mode-html-attr-equal-face    ((t (:inherit default))))
   `(web-mode-html-attr-name-face     ((t (:foreground ,nora-orange))))
   `(web-mode-html-tag-bracket-face   ((t (:inherit default))))
   `(web-mode-html-tag-face           ((t (:foreground ,nora-red))))
   `(web-mode-symbol-face             ((t (:foreground ,nora-orange))))

   ;; nxml
   `(nxml-attribute-local-name             ((t (:foreground ,nora-orange))))
   `(nxml-element-local-name               ((t (:foreground ,nora-red))))
   `(nxml-markup-declaration-delimiter     ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
   `(nxml-processing-instruction-delimiter ((t (:inherit nxml-markup-declaration-delimiter))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face          ((t (:foreground ,nora-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,nora-red))))
   `(rpm-spec-macro-face        ((t (:foreground ,nora-yellow))))
   `(rpm-spec-var-face          ((t (:foreground ,nora-red))))
   `(rpm-spec-doc-face          ((t (:foreground ,nora-purple))))
   `(rpm-spec-dir-face          ((t (:foreground ,nora-cyan))))
   `(rpm-spec-package-face      ((t (:foreground ,nora-red))))
   `(rpm-spec-ghost-face        ((t (:foreground ,nora-red))))
   `(rpm-spec-section-face      ((t (:foreground ,nora-yellow))))

   ;; guix
   `(guix-true ((t (:foreground ,nora-green :weight bold))))
   `(guix-build-log-phase-end ((t (:inherit success))))
   `(guix-build-log-phase-start ((t (:inherit success :weight bold))))

   ;; gomoku
   `(gomoku-O ((t (:foreground ,nora-red :weight bold))))
   `(gomoku-X ((t (:foreground ,nora-green :weight bold))))

   ;; tabbar
   `(tabbar-default             ((,class (:foreground ,nora-fg :background ,nora-black))))
   `(tabbar-highlight           ((,class (:underline t))))
   `(tabbar-button              ((,class (:foreground ,nora-fg :background ,nora-bg))))
   `(tabbar-button-highlight    ((,class (:inherit 'tabbar-button :inverse-video t))))
   `(tabbar-modified            ((,class (:inherit tabbar-button :foreground ,nora-purple :weight light :slant italic))))
   `(tabbar-unselected          ((,class (:inherit tabbar-default :foreground ,nora-fg :background ,nora-black :slant italic :underline nil :box (:line-width 1 :color ,nora-bg)))))
   `(tabbar-unselected-modified ((,class (:inherit tabbar-modified :background ,nora-black :underline nil :box (:line-width 1 :color ,nora-bg)))))
   `(tabbar-selected            ((,class (:inherit tabbar-default :foreground ,nora-fg :background ,nora-bg :weight bold :underline nil :box (:line-width 1 :color ,nora-bg)))))
   `(tabbar-selected-modified   ((,class (:inherit tabbar-selected :foreground ,nora-purple :underline nil :box (:line-width 1 :color ,nora-bg)))))

   ;; linum
   `(linum                    ((t (:foreground ,nora-gutter :background ,nora-bg))))
   ;; hlinum
   `(linum-highlight-face     ((t (:foreground ,nora-fg :background ,nora-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number              ((t (:foreground ,nora-silver :background ,nora-bg))))
   `(line-number-current-line ((t (:foreground ,nora-red :background ,nora-gutter))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,nora-gray))))
   `(reb-match-1 ((t (:background ,nora-black :foreground ,nora-purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,nora-black :foreground ,nora-green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,nora-black :foreground ,nora-yellow :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face       ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face             ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face    ((t (:underline (:color ,nora-red :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face              ((t (:inherit default))))

   ;; calendar
   `(diary   ((t (:inherit warning))))
   `(holiday ((t (:foreground ,nora-green))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,nora-orange))))
   `(breakpoint-enabled  ((t (:foreground ,nora-red :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,nora-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,nora-orange))   `(realgud-overlay-arrow2        ((t (:foreground ,nora-yellow))))
				    ))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,nora-red)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,nora-gray)))))
   `(realgud-line-number           ((t (:foreground ,nora-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; rmsbolt
   `(rmsbolt-current-line-face ((t (:inherit hl-line :weight bold))))

   ;; ruler-mode
   `(ruler-mode-column-number  ((t (:inherit ruler-mode-default))))
   `(ruler-mode-comment-column ((t (:foreground ,nora-red))))
   `(ruler-mode-current-column ((t (:foreground ,nora-accent :inherit ruler-mode-default))))
   `(ruler-mode-default        ((t (:inherit mode-line))))
   `(ruler-mode-fill-column    ((t (:foreground ,nora-orange :inherit ruler-mode-default))))
   `(ruler-mode-fringes        ((t (:foreground ,nora-green :inherit ruler-mode-default))))
   `(ruler-mode-goal-column    ((t (:foreground ,nora-cyan :inherit ruler-mode-default))))
   `(ruler-mode-margins        ((t (:inherit ruler-mode-default))))
   `(ruler-mode-tab-stop       ((t (:foreground ,nora-mono-3 :inherit ruler-mode-default))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face    ((t (:foreground ,nora-red))))
   `(undo-tree-visualizer-register-face   ((t (:foreground ,nora-orange))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,nora-cyan))))

   ;; tab-bar-mode
   `(tab-bar-tab-inactive ((t (:background ,nora-bg-hl :foreground ,nora-fg))))
   `(tab-bar-tab          ((t (:background ,nora-bg :foreground ,nora-purple))))
   `(tab-bar              ((t (:background ,nora-bg-hl))))

   ;; all-the-icons
   `(all-the-icons-purple    ((t (:foreground ,nora-purple))))
   `(all-the-icons-yellow    ((t (:foreground ,nora-yellow))))

   ;; dashboard
   `(dashboard-heading        ((t (:foreground ,nora-green))))
   `(dashboard-items-face     ((t (:bold ,nora-green))))

   ;; Language Customizations ----------------------------------------------------------------------
   ;; these laguage customizations are seperate from certain larger lagu

   ;; markdown
   `(markdown-header-face-1            ((t (:foreground ,nora-red :weight bold))))
   `(markdown-header-face-2            ((t (:foreground ,nora-orange :weight bold))))
   `(markdown-link-face                ((t (:foreground ,nora-purple ))))
   `(markdown-url-face                 ((t (:foreground ,nora-blue :underline t))))
   `(markdown-plain-url-face           ((t (:foreground ,nora-blue))))
   `(markdown-header-delimiter-face    ((t (:foreground ,nora-silver))))
   `(markdown-language-keyword-face    ((t (:foreground ,nora-green))))
   `(markdown-markup-face              ((t (:foreground ,nora-silver))))
   `(markdown-pre-face                 ((t (:foreground ,nora-green))))
   `(markdown-metadata-key-face        ((t (:foreground ,nora-green))))

   ;; org-mode
   `(org-date                  ((t (:foreground ,nora-cyan))))
   `(org-document-info         ((t (:foreground ,nora-mono-3))))
   `(org-document-info-keyword ((t (:inherit org-meta-line :underline t))))
   `(org-document-title        ((t (:weight bold))))
   `(org-footnote              ((t (:foreground ,nora-cyan))))
   `(org-sexp-date             ((t (:foreground ,nora-cyan))))
   `(org-table                 ((t (:foreground ,nora-blue))))
   `(org-drawer                ((t (:foreground ,nora-blue))))
   `(org-headline-done         ((t (:foreground ,nora-purple))))
   ;; `(org-block                 ((t (:background ,nora-bg-hl :foreground ,nora-silver :extend t))))
   ;; `(org-block-begin-line      ((t (:background ,nora-bg-hl :foreground ,nora-silver :extend t))))
   ;; `(org-block-end-line        ((t (:background ,nora-bg-hl :foreground ,nora-silver :extend t))))
   `(org-level-1               ((t (:foreground ,nora-green))))
   `(org-level-2               ((t (:foreground ,nora-red))))
   `(org-level-3               ((t (:foreground ,nora-purple))))
   `(org-level-4               ((t (:foreground ,nora-orange))))
   `(org-level-6               ((t (:foreground ,nora-blue))))
   `(org-level-7               ((t (:foreground ,nora-silver))))
   `(org-level-8               ((t (:foreground ,nora-cyan))))
   `(org-cite                  ((t (:foreground ,nora-blue :weight bold))))
   `(org-cite-key              ((t (:foreground ,nora-green :weight bold))))
   `(org-hide                  ((t (:foreground ,nora-bg))))

   ;; latex-mode
   `(font-latex-sectioning-0-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-sectioning-1-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-sectioning-2-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-sectioning-3-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-sectioning-4-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-sectioning-5-face        ((t (:foreground ,nora-blue :height 1.0))))
   `(font-latex-bold-face                ((t (:foreground ,nora-green :weight bold))))
   `(font-latex-italic-face              ((t (:foreground ,nora-green :slant italic))))
   `(font-latex-warning-face             ((t (:foreground ,nora-red))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,nora-cyan))))
   `(font-latex-script-char-face         ((t (:foreground ,nora-gray))))

   ;; sh-mode
   `(sh-heredoc ((t (:inherit font-lock-string-face :slant italic))))

   ;; js2-mode
   `(js2-error             ((t (:underline (:color ,nora-red :style wave)))))
   `(js2-external-variable ((t (:foreground ,nora-cyan))))
   `(js2-warning           ((t (:underline (:color ,nora-orange :style wave)))))
   `(js2-function-call     ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param    ((t (:foreground ,nora-mono-1))))
   `(js2-jsdoc-tag         ((t (:foreground ,nora-purple))))
   `(js2-jsdoc-type        ((t (:foreground ,nora-yellow))))
   `(js2-jsdoc-value       ((t (:foreground ,nora-red))))
   `(js2-object-property   ((t (:foreground ,nora-red))))

   ;; whitespace-mode
   `(whitespace-big-indent       ((t (:foreground ,nora-border))))
   `(whitespace-empty            ((t (:foreground ,nora-border))))
   `(whitespace-hspace           ((t (:foreground ,nora-border))))
   `(whitespace-indentation      ((t (:foreground ,nora-border))))
   `(whitespace-line             ((t (:background ,nora-border))))
   `(whitespace-newline          ((t (:foreground ,nora-border))))
   `(whitespace-space            ((t (:foreground ,nora-border))))
   `(whitespace-space-after-tab  ((t (:foreground ,nora-border))))
   `(whitespace-space-before-tab ((t (:foreground ,nora-border))))
   `(whitespace-tab              ((t (:foreground ,nora-border))))
   `(whitespace-trailing         ((t (:foreground ,nora-silver))))
   ))

(nora-with-color-variables
  (custom-theme-set-variables
   'nora
   ;; fill-column-indicator
   `(fci-rule-color ,nora-gray)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `nora-yellow' |
   ;; | J         | `nora-blue'     |
   ;; | L         | `nora-orange' |
   ;; | Z         | `nora-red'    |
   ;; | S         | `nora-green'    |
   ;; | T         | `nora-purple'   |
   ;; | I         | `nora-cyan'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,nora-black ,nora-red ,nora-green ,nora-yellow
				  ,nora-blue ,nora-purple ,nora-cyan ,nora-fg])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'nora)
(provide 'nora-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; nora-theme.el ends here
