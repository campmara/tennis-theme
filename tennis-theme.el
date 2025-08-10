(deftheme tennis
  "tennis - A custom emacs theme for Mara based on her personal preferences.")

(defvar tennis-colors-alist
    (let ((colors `(("tennis-accent"   . "#a7c080")
                    ("tennis-fg"       . "#c4d3aa")
                    ("tennis-bg"       . "#173030")
                    ("tennis-bg-1"     . "#325252")
                    ("tennis-bg-hl"    . "#3a454a")
                    ("tennis-gutter"   . "#142424")
                    ("tennis-mono-1"   . "#abb2bf")
                    ("tennis-mono-2"   . "#503946")
                    ("tennis-mono-3"   . "#36e381")
                    ("tennis-cyan"     . "#83c092")
                    ("tennis-blue"     . "#7fbbb3")
                    ("tennis-purple"   . "#d699b6")
                    ("tennis-green"    . "#a7c080")
                    ("tennis-red"      . "#e67e80")
                    ("tennis-orange"   . "#e69875")
                    ("tennis-yellow"   . "#ddbc7f")
                    ("tennis-gray"     . "#325252")
                    ("tennis-silver"   . "#9da9a0")
                    ("tennis-black"    . "#173030")
                    ("tennis-border"   . "#142424")
                    ("tennis-visual"   . "#1c4040"))))
        colors)
    "List of tennis Colors."
)

(defmacro tennis-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
     ,@ (mapcar (lambda (cons)
              (list (intern (car cons)) (cdr cons)))
            tennis-colors-alist))
     ,@body))

(tennis-with-color-variables
  (custom-theme-set-faces
   'tennis

   `(default             ((t (:foreground ,tennis-fg :background ,tennis-bg))))
   `(success             ((t (:foreground ,tennis-green))))
   `(warning             ((t (:foreground ,tennis-yellow))))
   `(error               ((t (:foreground ,tennis-red :weight bold))))
   `(link                ((t (:foreground ,tennis-blue :underline t :weight bold))))
   `(link-visited        ((t (:foreground ,tennis-blue :underline t :weight normal))))
   `(cursor              ((t (:background ,tennis-fg))))
   `(fringe              ((t (:background ,tennis-bg :foreground ,tennis-silver))))
   `(region              ((t (:background ,tennis-gutter :distant-foreground ,tennis-mono-2))))
   `(highlight           ((t (:background ,tennis-gray :distant-foreground ,tennis-mono-2))))
   `(hl-line             ((t (:background ,tennis-visual :distant-foreground nil))))
   `(header-line         ((t (:background ,tennis-black))))
   `(vertical-border     ((t (:background ,tennis-border :foreground ,tennis-border))))
   `(secondary-selection ((t (:background ,tennis-bg-1))))
   `(query-replace       ((t (:inherit (isearch)))))
   `(minibuffer-prompt   ((t (:foreground ,tennis-blue))))
   `(tooltip             ((t (:foreground ,tennis-fg :background ,tennis-bg-1 :inherit variable-pitch))))

   `(font-lock-builtin-face           ((t (:foreground ,tennis-cyan))))
   `(font-lock-comment-face           ((t (:foreground ,tennis-mono-3 :slant normal))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face               ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face     ((t (:foreground ,tennis-green))))
   `(font-lock-keyword-face           ((t (:foreground ,tennis-red :weight normal))))
   `(font-lock-preprocessor-face      ((t (:foreground ,tennis-red))))
   `(font-lock-string-face            ((t (:foreground ,tennis-green))))
   `(font-lock-type-face              ((t (:foreground ,tennis-green))))
   `(font-lock-constant-face          ((t (:foreground ,tennis-cyan))))
   `(font-lock-variable-name-face     ((t (:foreground ,tennis-blue))))
   `(font-lock-warning-face           ((t (:foreground ,tennis-mono-3 :bold t))))
   `(font-lock-negation-char-face     ((t (:foreground ,tennis-cyan :bold t))))
   `(highlight-numbers-number         ((t (:foreground ,tennis-purple))))

   ;; eob
   `(vi-tilde-fringe-face ((t (:foreground ,tennis-silver))))
   `(solaire-fringe-face  ((t (:foreground ,tennis-silver))))

   ;; mode-line
   `(mode-line           ((t (:background ,tennis-gutter :foreground ,tennis-silver))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis  ((t (:weight bold))))
   `(mode-line-inactive  ((t (:background ,tennis-visual :foreground ,tennis-silver))))

   ;; window-divider
   `(window-divider             ((t (:foreground ,tennis-border))))
   `(window-divider-first-pixel ((t (:foreground ,tennis-border))))
   `(window-divider-last-pixel  ((t (:foreground ,tennis-border))))

   ;; custom
   `(custom-state ((t (:foreground ,tennis-green))))

   ;; Package/Plugin Customizations

   ;; ido
   `(ido-first-match ((t (:foreground ,tennis-purple :weight bold))))
   `(ido-only-match  ((t (:foreground ,tennis-red :weight bold))))
   `(ido-subdir      ((t (:foreground ,tennis-blue))))
   `(ido-virtual     ((t (:foreground ,tennis-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,tennis-mono-3 :background ,tennis-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,tennis-red :background ,tennis-bg-1 :inverse-video nil))))

   ;; ace-window
   `(aw-background-face   ((t (:inherit font-lock-comment-face))))
   `(aw-leading-char-face ((t (:foreground ,tennis-red :weight bold))))

   ;; centaur-tabs
   `(centaur-tabs-default           ((t (:background ,tennis-black :foreground ,tennis-black))))
   `(centaur-tabs-selected          ((t (:background ,tennis-bg :foreground ,tennis-fg :weight bold))))
   `(centaur-tabs-unselected        ((t (:background ,tennis-black :foreground ,tennis-fg :weight light))))
   `(centaur-tabs-selected-modified ((t (:background ,tennis-bg
                     :foreground ,tennis-blue :weight bold))))
   `(centaur-tabs-unselected-modified ((t (:background ,tennis-black :weight light
                       :foreground ,tennis-blue))))
   `(centaur-tabs-active-bar-face            ((t (:background ,tennis-accent))))
   `(centaur-tabs-modified-marker-selected   ((t (:inherit 'centaur-tabs-selected :foreground,tennis-accent))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected :foreground,tennis-accent))))

   ;; company-mode
   `(company-tooltip                      ((t (:foreground ,tennis-fg :background ,tennis-gray))))
   `(company-tooltip-annotation           ((t (:foreground ,tennis-mono-2 :background ,tennis-gray))))
   `(company-tooltip-annotation-selection ((t (:foreground ,tennis-mono-2 :background ,tennis-mono-3))))
   `(company-tooltip-selection            ((t (:foreground ,tennis-fg :background ,tennis-mono-3))))
   `(company-tooltip-mouse                ((t (:background ,tennis-gray))))
   `(company-tooltip-common               ((t (:foreground ,tennis-yellow :background ,tennis-gray))))
   `(company-tooltip-common-selection     ((t (:foreground ,tennis-yellow :background ,tennis-mono-3))))
   `(company-preview                      ((t (:background ,tennis-bg))))
   `(company-preview-common               ((t (:foreground ,tennis-yellow :background ,tennis-bg))))
   `(company-scrollbar-fg                 ((t (:background ,tennis-mono-1))))
   `(company-scrollbar-bg                 ((t (:background ,tennis-gray))))
   `(company-template-field               ((t (:inherit highlight))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,tennis-accent))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,tennis-cyan :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,tennis-red :style wave)))))

   ;; flymake
   `(flymake-error   ((t (:underline (:color ,tennis-red :style wave)))))
   `(flymake-note    ((t (:underline (:color ,tennis-green :style wave)))))
   `(flymake-warning ((t (:underline (:color ,tennis-orange :style wave)))))

   ;; flycheck
   `(flycheck-error   ((t (:underline (:color ,tennis-red :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,tennis-green :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,tennis-orange :style wave)))))

   ;; compilation
   `(compilation-face           ((t (:foreground ,tennis-fg))))
   `(compilation-line-number    ((t (:foreground ,tennis-mono-2))))
   `(compilation-column-number  ((t (:foreground ,tennis-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch        ((t (:foreground ,tennis-bg :background ,tennis-purple))))
   `(isearch-fail   ((t (:foreground ,tennis-red :background nil))))
   `(lazy-highlight ((t (:foreground ,tennis-purple :background ,tennis-bg-1 :underline ,tennis-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged       ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink       ((t (:foreground "#FD5FF1"))))
   `(diredfl-file-name   ((t (:foreground ,tennis-fg))))
   `(diredfl-file-suffix ((t (:foreground ,tennis-fg))))
   `(diredfl-number      ((t (:foreground ,tennis-red))))
   `(diredfl-date-time   ((t (:foreground ,tennis-blue))))
   `(diredfl-no-priv     ((t (:foreground ,tennis-yellow))))
   `(diredfl-dir-priv    ((t (:foreground ,tennis-yellow))))
   `(diredfl-read-priv   ((t (:foreground ,tennis-yellow))))
   `(diredfl-write-priv  ((t (:foreground ,tennis-yellow))))
   `(diredfl-exec-priv   ((t (:foreground ,tennis-yellow))))
   `(diredfl-link-priv   ((t (:foreground ,tennis-yellow))))
   `(diredfl-dir-heading ((t (:foreground ,tennis-green :weight bold))))

   ;; dired-async
   `(dired-async-failures     ((t (:inherit error))))
   `(dired-async-message      ((t (:inherit success))))
   `(dired-async-mode-message ((t (:foreground ,tennis-orange))))

   ;; helm
   `(helm-header ((t (:foreground ,tennis-mono-2
              :background ,tennis-bg
              :underline nil
              :box (:line-width 6 :color ,tennis-bg)))))
   `(helm-source-header ((t (:foreground ,tennis-yellow
                 :background ,tennis-bg
                 :underline nil
                 :weight bold
                 :box (:line-width 6 :color ,tennis-bg)))))
   `(helm-selection                    ((t (:background ,tennis-gray))))
   `(helm-selection-line               ((t (:background ,tennis-gray))))
   `(helm-visible-mark                 ((t (:background ,tennis-bg :foreground ,tennis-yellow))))
   `(helm-candidate-number             ((t (:foreground ,tennis-green :background ,tennis-bg-1))))
   `(helm-separator                    ((t (:background ,tennis-bg :foreground ,tennis-red))))
   `(helm-M-x-key                      ((t (:foreground ,tennis-orange))))
   `(helm-bookmark-addressbook         ((t (:foreground ,tennis-orange))))
   `(helm-bookmark-directory           ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file                ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus                ((t (:foreground ,tennis-purple))))
   `(helm-bookmark-info                ((t (:foreground ,tennis-green))))
   `(helm-bookmark-man                 ((t (:foreground ,tennis-yellow))))
   `(helm-bookmark-w3m                 ((t (:foreground ,tennis-purple))))
   `(helm-match                        ((t (:foreground ,tennis-yellow))))
   `(helm-ff-directory                 ((t (:foreground ,tennis-cyan :background ,tennis-bg :weight bold))))
   `(helm-ff-file                      ((t (:foreground ,tennis-fg :background ,tennis-bg :weight normal))))
   `(helm-ff-executable                ((t (:foreground ,tennis-green :background ,tennis-bg :weight normal))))
   `(helm-ff-invalid-symlink           ((t (:foreground ,tennis-red :background ,tennis-bg :weight bold))))
   `(helm-ff-symlink                   ((t (:foreground ,tennis-yellow :background ,tennis-bg :weight bold))))
   `(helm-ff-prefix                    ((t (:foreground ,tennis-bg :background ,tennis-yellow :weight normal))))
   `(helm-buffer-not-saved             ((t (:foreground ,tennis-red))))
   `(helm-buffer-process               ((t (:foreground ,tennis-mono-2))))
   `(helm-buffer-saved-out             ((t (:foreground ,tennis-fg))))
   `(helm-buffer-size                  ((t (:foreground ,tennis-mono-2))))
   `(helm-buffer-directory             ((t (:foreground ,tennis-purple))))
   `(helm-grep-cmd-line                ((t (:foreground ,tennis-cyan))))
   `(helm-grep-file                    ((t (:foreground ,tennis-fg))))
   `(helm-grep-finish                  ((t (:foreground ,tennis-green))))
   `(helm-grep-lineno                  ((t (:foreground ,tennis-mono-2))))
   `(helm-grep-finish                  ((t (:foreground ,tennis-red))))
   `(helm-grep-match                   ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,tennis-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face       ((t (:background ,tennis-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face       ((t (:background ,tennis-purple :foreground "#ffffff"))))
   `(helm-locate-finish                ((t (:foreground ,tennis-green))))
   `(info-menu-star                    ((t (:foreground ,tennis-red))))

   ;; ivy
   `(ivy-confirm-face               ((t (:inherit minibuffer-prompt :foreground ,tennis-green))))
   `(ivy-current-match              ((t (:background ,tennis-gray :weight normal))))
   `(ivy-highlight-face             ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face        ((t (:inherit minibuffer-prompt :foreground ,tennis-red))))
   `(ivy-minibuffer-match-face-1    ((t (:background ,tennis-bg-hl))))
   `(ivy-minibuffer-match-face-2    ((t (:inherit ivy-minibuffer-match-face-1 :background ,tennis-black :foreground ,tennis-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3    ((t (:inherit ivy-minibuffer-match-face-2 :background ,tennis-black :foreground ,tennis-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4    ((t (:inherit ivy-minibuffer-match-face-2 :background ,tennis-black :foreground ,tennis-yellow :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:inherit ivy-current-match))))
   `(ivy-modified-buffer            ((t (:inherit default :foreground ,tennis-orange))))
   `(ivy-virtual                    ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,tennis-yellow :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,tennis-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,tennis-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,tennis-yellow :weight bold))))

   ;; git-gutter
   `(git-gutter:added    ((t (:foreground ,tennis-green :weight bold))))
   `(git-gutter:deleted  ((t (:foreground ,tennis-red :weight bold))))
   `(git-gutter:modified ((t (:foreground ,tennis-orange :weight bold))))

   ;; eshell
   `(eshell-ls-archive    ((t (:foreground ,tennis-purple :weight bold))))
   `(eshell-ls-backup     ((t (:foreground ,tennis-yellow))))
   `(eshell-ls-clutter    ((t (:foreground ,tennis-red :weight bold))))
   `(eshell-ls-directory  ((t (:foreground ,tennis-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,tennis-green :weight bold))))
   `(eshell-ls-missing    ((t (:foreground ,tennis-red :weight bold))))
   `(eshell-ls-product    ((t (:foreground ,tennis-yellow))))
   `(eshell-ls-special    ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink    ((t (:foreground ,tennis-cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,tennis-mono-1))))
   `(eshell-prompt        ((t (:inherit minibuffer-prompt))))

   ;; man
   `(Man-overstrike ((t (:foreground ,tennis-green :weight bold))))
   `(Man-underline  ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold   ((t (:foreground ,tennis-green :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face     ((t (:inherit widget-button))))
   `(dictionary-reference-face  ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; erc
   `(erc-error-face     ((t (:inherit error))))
   `(erc-input-face     ((t (:inherit shadow))))
   `(erc-my-nick-face   ((t (:foreground ,tennis-accent))))
   `(erc-notice-face    ((t (:inherit font-lock-comment-face))))
   `(erc-timestamp-face ((t (:foreground ,tennis-green :weight bold))))

   ;; jabber
   `(jabber-roster-user-online     ((t (:foreground ,tennis-green))))
   `(jabber-roster-user-away       ((t (:foreground ,tennis-red))))
   `(jabber-roster-user-xa         ((t (:foreground ,tennis-red))))
   `(jabber-roster-user-dnd        ((t (:foreground ,tennis-purple))))
   `(jabber-roster-user-chatty     ((t (:foreground ,tennis-yellow))))
   `(jabber-roster-user-error      ((t (:foreground ,tennis-red :bold t))))
   `(jabber-roster-user-offline    ((t (:foreground ,tennis-mono-3))))
   `(jabber-chat-prompt-local      ((t (:foreground ,tennis-blue))))
   `(jabber-chat-prompt-foreign    ((t (:foreground ,tennis-yellow))))
   `(jabber-chat-prompt-system     ((t (:foreground ,tennis-mono-3))))
   `(jabber-chat-error             ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face         ((t (:foreground ,tennis-cyan))))
   `(jabber-activity-face          ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; eww
   `(eww-form-checkbox       ((t (:inherit eww-form-submit))))
   `(eww-form-file           ((t (:inherit eww-form-submit))))
   `(eww-form-select         ((t (:inherit eww-form-submit))))
   `(eww-form-submit         ((t (:background ,tennis-gray :foreground ,tennis-fg :box (:line-width 2 :color ,tennis-border :style released-button)))))
   `(eww-form-text           ((t (:inherit widget-field :box (:line-width 1 :color ,tennis-border)))))
   `(eww-form-textarea       ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,tennis-red))))
   `(eww-valid-certificate   ((t (:foreground ,tennis-green))))

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
   `(magit-section-highlight           ((t (:background ,tennis-bg-hl))))
   `(magit-section-heading             ((t (:foreground ,tennis-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,tennis-fg :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,tennis-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,tennis-yellow :background ,tennis-bg-hl :weight bold))))
   `(magit-diff-hunk-heading           ((t (:foreground ,tennis-mono-2 :background ,tennis-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,tennis-mono-1 :background ,tennis-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,tennis-purple :background ,tennis-mono-3))))
   `(magit-diff-context                ((t (:foreground ,tennis-fg))))
   `(magit-diff-context-highlight      ((t (:background ,tennis-bg-1 :foreground ,tennis-fg))))
   `(magit-diffstat-added              ((t (:foreground ,tennis-green))))
   `(magit-diffstat-removed            ((t (:foreground ,tennis-red))))
   `(magit-process-ok                  ((t (:foreground ,tennis-green))))
   `(magit-process-ng                  ((t (:foreground ,tennis-red))))
   `(magit-log-author                  ((t (:foreground ,tennis-yellow))))
   `(magit-log-date                    ((t (:foreground ,tennis-mono-2))))
   `(magit-log-graph                   ((t (:foreground ,tennis-silver))))
   `(magit-sequence-pick               ((t (:foreground ,tennis-yellow))))
   `(magit-sequence-stop               ((t (:foreground ,tennis-green))))
   `(magit-sequence-part               ((t (:foreground ,tennis-orange))))
   `(magit-sequence-head               ((t (:foreground ,tennis-blue))))
   `(magit-sequence-drop               ((t (:foreground ,tennis-red))))
   `(magit-sequence-done               ((t (:foreground ,tennis-mono-2))))
   `(magit-sequence-onto               ((t (:foreground ,tennis-mono-2))))
   `(magit-bisect-good                 ((t (:foreground ,tennis-green))))
   `(magit-bisect-skip                 ((t (:foreground ,tennis-orange))))
   `(magit-bisect-bad                  ((t (:foreground ,tennis-red))))
   `(magit-blame-heading               ((t (:background ,tennis-bg-1 :foreground ,tennis-mono-2))))
   `(magit-blame-hash                  ((t (:background ,tennis-bg-1 :foreground ,tennis-purple))))
   `(magit-blame-name                  ((t (:background ,tennis-bg-1 :foreground ,tennis-yellow))))
   `(magit-blame-date                  ((t (:background ,tennis-bg-1 :foreground ,tennis-mono-3))))
   `(magit-blame-summary               ((t (:background ,tennis-bg-1 :foreground ,tennis-mono-2))))
   `(magit-dimmed                      ((t (:foreground ,tennis-mono-2))))
   `(magit-hash                        ((t (:foreground ,tennis-purple))))
   `(magit-tag                         ((t (:foreground ,tennis-orange :weight bold))))
   `(magit-branch-remote               ((t (:foreground ,tennis-green :weight bold))))
   `(magit-branch-local                ((t (:foreground ,tennis-blue :weight bold))))
   `(magit-branch-current              ((t (:foreground ,tennis-blue :weight bold :box t))))
   `(magit-head                        ((t (:foreground ,tennis-blue :weight bold))))
   `(magit-refname                     ((t (:background ,tennis-bg :foreground ,tennis-fg :weight bold))))
   `(magit-refname-stash               ((t (:background ,tennis-bg :foreground ,tennis-fg :weight bold))))
   `(magit-refname-wip                 ((t (:background ,tennis-bg :foreground ,tennis-fg :weight bold))))
   `(magit-signature-good              ((t (:foreground ,tennis-green))))
   `(magit-signature-bad               ((t (:foreground ,tennis-red))))
   `(magit-signature-untrusted         ((t (:foreground ,tennis-orange))))
   `(magit-cherry-unmatched            ((t (:foreground ,tennis-cyan))))
   `(magit-cherry-equivalent           ((t (:foreground ,tennis-purple))))
   `(magit-reflog-commit               ((t (:foreground ,tennis-green))))
   `(magit-reflog-amend                ((t (:foreground ,tennis-purple))))
   `(magit-reflog-merge                ((t (:foreground ,tennis-green))))
   `(magit-reflog-checkout             ((t (:foreground ,tennis-blue))))
   `(magit-reflog-reset                ((t (:foreground ,tennis-red))))
   `(magit-reflog-rebase               ((t (:foreground ,tennis-purple))))
   `(magit-reflog-cherry-pick          ((t (:foreground ,tennis-green))))
   `(magit-reflog-remote               ((t (:foreground ,tennis-cyan))))
   `(magit-reflog-other                ((t (:foreground ,tennis-cyan))))

   ;; message
   `(message-cited-text         ((t (:foreground ,tennis-green))))
   `(message-header-cc          ((t (:foreground ,tennis-orange :weight bold))))
   `(message-header-name        ((t (:foreground ,tennis-purple))))
   `(message-header-newsgroups  ((t (:foreground ,tennis-yellow :weight bold :slant italic))))
   `(message-header-other       ((t (:foreground ,tennis-red))))
   `(message-header-subject     ((t (:foreground ,tennis-blue))))
   `(message-header-to          ((t (:foreground ,tennis-yellow :weight bold))))
   `(message-header-xheader     ((t (:foreground ,tennis-silver))))
   `(message-mml                ((t (:foreground ,tennis-purple))))
   `(message-separator          ((t (:foreground ,tennis-mono-3 :slant italic))))

   ;; epa
   `(epa-field-body ((t (:foreground ,tennis-blue :slant italic))))
   `(epa-field-name ((t (:foreground ,tennis-cyan :weight bold))))

   ;; notmuch
   `(notmuch-crypto-decryption            ((t (:foreground ,tennis-purple :background ,tennis-black))))
   `(notmuch-crypto-signature-bad         ((t (:foreground ,tennis-red :background ,tennis-black))))
   `(notmuch-crypto-signature-good        ((t (:foreground ,tennis-green :background ,tennis-black))))
   `(notmuch-crypto-signature-good-key    ((t (:foreground ,tennis-green :background ,tennis-black))))
   `(notmuch-crypto-signature-unknown     ((t (:foreground ,tennis-orange :background ,tennis-black))))
   `(notmuch-hello-logo-background        ((t (:inherit default))))
   `(notmuch-message-summary-face         ((t (:background ,tennis-black))))
   `(notmuch-search-count                 ((t (:inherit default :foreground ,tennis-silver))))
   `(notmuch-search-date                  ((t (:inherit default :foreground ,tennis-purple))))
   `(notmuch-search-matching-authors      ((t (:inherit default :foreground ,tennis-yellow))))
   `(notmuch-search-non-matching-authors  ((t (:inherit font-lock-comment-face :slant italic))))
   `(notmuch-tag-added                    ((t (:underline t))))
   `(notmuch-tag-deleted                  ((t (:strike-through ,tennis-red))))
   `(notmuch-tag-face                     ((t (:foreground ,tennis-green))))
   `(notmuch-tag-unread                   ((t (:foreground ,tennis-red))))
   `(notmuch-tree-match-author-face       ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face         ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face              ((t (:weight semi-bold))))
   `(notmuch-tree-match-tag-face          ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-face           ((t (:slant italic :weight light :inherit font-lock-comment-face))))

   ;; mu4e
   `(mu4e-header-key-face      ((t (:foreground ,tennis-green :weight bold))))
   `(mu4e-header-title-face    ((t (:foreground ,tennis-blue))))
   `(mu4e-title-face           ((t (:foreground ,tennis-green :weight bold))))

   ;; calendar
   `(cfw:face-title                  ((t (:foreground ,tennis-green :weight bold))))
   `(cfw:face-select                 ((t (:foreground ,tennis-bg :background ,tennis-fg))))
   `(cfw:face-header                 ((t (:foreground ,tennis-fg :weight bold))))
   `(cfw:face-sunday                 ((t (:foreground ,tennis-green :weight bold))))
   `(cfw:face-holiday                ((t (:foreground ,tennis-blue :weight bold))))
   `(cfw:face-toolbar                ((t (:background ,tennis-bg))))
   `(cfw:face-toolbar-button-on      ((t (:foreground ,tennis-fg :weight bold))))
   `(cfw:face-toolbar-button-off     ((t (:foreground ,tennis-silver :weight bold))))
   `(cfw:face-day-title              ((t (:background ,tennis-bg))))
   `(cfw:face-today-title            ((t (:foreground ,tennis-bg :background ,tennis-green))))

   ;; elfeed
   `(elfeed-log-debug-level-face      ((t (:background ,tennis-black :foreground ,tennis-green))))
   `(elfeed-log-error-level-face      ((t (:background ,tennis-black :foreground ,tennis-red))))
   `(elfeed-log-info-level-face       ((t (:background ,tennis-black :foreground ,tennis-blue))))
   `(elfeed-log-warn-level-face       ((t (:background ,tennis-black :foreground ,tennis-orange))))
   `(elfeed-search-date-face          ((t (:foreground ,tennis-purple))))
   `(elfeed-search-feed-face          ((t (:foreground ,tennis-yellow))))
   `(elfeed-search-tag-face           ((t (:foreground ,tennis-green))))
   `(elfeed-search-title-face         ((t (:foreground ,tennis-silver))))
   `(elfeed-search-unread-title-face  ((t (:foreground ,tennis-mono-1 :weight bold))))
   `(elfeed-search-unread-count-face  ((t (:foreground ,tennis-silver))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,tennis-blue))))

   ;; powerline
   `(powerline-active1    ((,class (:background ,tennis-bg-hl :foreground ,tennis-purple))))
   `(powerline-active2    ((,class (:background ,tennis-bg-hl :foreground ,tennis-purple))))
   `(powerline-inactive1  ((,class (:background ,tennis-bg :foreground ,tennis-fg))))
   `(powerline-inactive2  ((,class (:background ,tennis-bg :foreground ,tennis-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,tennis-green))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,tennis-red))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,tennis-blue))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,tennis-cyan))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,tennis-purple))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,tennis-yellow))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,tennis-orange))))
   `(rainbow-delimiters-depth-8-face    ((t (:foreground ,tennis-green))))
   `(rainbow-delimiters-depth-9-face    ((t (:foreground ,tennis-orange))))
   `(rainbow-delimiters-depth-10-face   ((t (:foreground ,tennis-cyan))))
   `(rainbow-delimiters-depth-11-face   ((t (:foreground ,tennis-purple))))
   `(rainbow-delimiters-depth-12-face   ((t (:foreground ,tennis-yellow))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,tennis-red :weight bold))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,tennis-green))))

   ;; elixir
   `(elixir-atom-face       ((t (:foreground ,tennis-cyan))))
   `(elixir-attribute-face  ((t (:foreground ,tennis-red))))

   ;; show-paren
   `(show-paren-match     ((,class (:foreground ,tennis-purple :inherit bold :underline t))))
   `(show-paren-mismatch  ((,class (:foreground ,tennis-red :inherit bold :underline t))))

   ;; cider
   `(cider-fringe-good-face ((t (:foreground ,tennis-green))))

   ;; sly
   `(sly-error-face          ((t (:underline (:color ,tennis-red :style wave)))))
   `(sly-mrepl-note-face     ((t (:inherit font-lock-comment-face))))
   `(sly-mrepl-output-face   ((t (:inherit font-lock-string-face))))
   `(sly-mrepl-prompt-face   ((t (:inherit comint-highlight-prompt))))
   `(sly-note-face           ((t (:underline (:color ,tennis-green :style wave)))))
   `(sly-style-warning-face  ((t (:underline (:color ,tennis-yellow :style wave)))))
   `(sly-warning-face        ((t (:underline (:color ,tennis-orange :style wave)))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,tennis-red :background ,tennis-gray :weight bold))))
   `(sp-show-pair-match-face    ((t (:foreground ,tennis-blue :weight bold :underline t))))

   ;; lispy
   `(lispy-face-hint ((t (:background ,tennis-border :foreground ,tennis-yellow))))

   ;; lispyville
   `(lispyville-special-face ((t (:foreground ,tennis-red))))

   ;; spaceline
   `(spaceline-flycheck-error    ((,class (:foreground ,tennis-red))))
   `(spaceline-flycheck-info     ((,class (:foreground ,tennis-green))))
   `(spaceline-flycheck-warning  ((,class (:foreground ,tennis-orange))))
   `(spaceline-python-venv       ((,class (:foreground ,tennis-purple))))

   ;; solaire mode
   `(solaire-default-face      ((,class (:inherit default :background ,tennis-black))))
   `(solaire-minibuffer-face   ((,class (:inherit default :background ,tennis-black))))

   ;; web-mode
   `(web-mode-doctype-face            ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face              ((t (:background ,tennis-black :foreground ,tennis-red))))
   `(web-mode-html-attr-equal-face    ((t (:inherit default))))
   `(web-mode-html-attr-name-face     ((t (:foreground ,tennis-orange))))
   `(web-mode-html-tag-bracket-face   ((t (:inherit default))))
   `(web-mode-html-tag-face           ((t (:foreground ,tennis-red))))
   `(web-mode-symbol-face             ((t (:foreground ,tennis-orange))))

   ;; nxml
   `(nxml-attribute-local-name             ((t (:foreground ,tennis-orange))))
   `(nxml-element-local-name               ((t (:foreground ,tennis-red))))
   `(nxml-markup-declaration-delimiter     ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
   `(nxml-processing-instruction-delimiter ((t (:inherit nxml-markup-declaration-delimiter))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face          ((t (:foreground ,tennis-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,tennis-red))))
   `(rpm-spec-macro-face        ((t (:foreground ,tennis-yellow))))
   `(rpm-spec-var-face          ((t (:foreground ,tennis-red))))
   `(rpm-spec-doc-face          ((t (:foreground ,tennis-purple))))
   `(rpm-spec-dir-face          ((t (:foreground ,tennis-cyan))))
   `(rpm-spec-package-face      ((t (:foreground ,tennis-red))))
   `(rpm-spec-ghost-face        ((t (:foreground ,tennis-red))))
   `(rpm-spec-section-face      ((t (:foreground ,tennis-yellow))))

   ;; guix
   `(guix-true ((t (:foreground ,tennis-green :weight bold))))
   `(guix-build-log-phase-end ((t (:inherit success))))
   `(guix-build-log-phase-start ((t (:inherit success :weight bold))))

   ;; gomoku
   `(gomoku-O ((t (:foreground ,tennis-red :weight bold))))
   `(gomoku-X ((t (:foreground ,tennis-green :weight bold))))

   ;; tabbar
   `(tabbar-default             ((,class (:foreground ,tennis-fg :background ,tennis-black))))
   `(tabbar-highlight           ((,class (:underline t))))
   `(tabbar-button              ((,class (:foreground ,tennis-fg :background ,tennis-bg))))
   `(tabbar-button-highlight    ((,class (:inherit 'tabbar-button :inverse-video t))))
   `(tabbar-modified            ((,class (:inherit tabbar-button :foreground ,tennis-purple :weight light :slant italic))))
   `(tabbar-unselected          ((,class (:inherit tabbar-default :foreground ,tennis-fg :background ,tennis-black :slant italic :underline nil :box (:line-width 1 :color ,tennis-bg)))))
   `(tabbar-unselected-modified ((,class (:inherit tabbar-modified :background ,tennis-black :underline nil :box (:line-width 1 :color ,tennis-bg)))))
   `(tabbar-selected            ((,class (:inherit tabbar-default :foreground ,tennis-fg :background ,tennis-bg :weight bold :underline nil :box (:line-width 1 :color ,tennis-bg)))))
   `(tabbar-selected-modified   ((,class (:inherit tabbar-selected :foreground ,tennis-purple :underline nil :box (:line-width 1 :color ,tennis-bg)))))

   ;; linum
   `(linum                    ((t (:foreground ,tennis-gutter :background ,tennis-bg))))
   ;; hlinum
   `(linum-highlight-face     ((t (:foreground ,tennis-fg :background ,tennis-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number              ((t (:foreground ,tennis-silver :background ,tennis-bg))))
   `(line-number-current-line ((t (:foreground ,tennis-red :background ,tennis-gutter))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,tennis-gray))))
   `(reb-match-1 ((t (:background ,tennis-black :foreground ,tennis-purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,tennis-black :foreground ,tennis-green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,tennis-black :foreground ,tennis-yellow :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face       ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face             ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face    ((t (:underline (:color ,tennis-red :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face              ((t (:inherit default))))

   ;; calendar
   `(diary   ((t (:inherit warning))))
   `(holiday ((t (:foreground ,tennis-green))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,tennis-orange))))
   `(breakpoint-enabled  ((t (:foreground ,tennis-red :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,tennis-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,tennis-orange))   `(realgud-overlay-arrow2        ((t (:foreground ,tennis-yellow))))
                    ))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,tennis-red)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,tennis-gray)))))
   `(realgud-line-number           ((t (:foreground ,tennis-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; rmsbolt
   `(rmsbolt-current-line-face ((t (:inherit hl-line :weight bold))))

   ;; ruler-mode
   `(ruler-mode-column-number  ((t (:inherit ruler-mode-default))))
   `(ruler-mode-comment-column ((t (:foreground ,tennis-red))))
   `(ruler-mode-current-column ((t (:foreground ,tennis-accent :inherit ruler-mode-default))))
   `(ruler-mode-default        ((t (:inherit mode-line))))
   `(ruler-mode-fill-column    ((t (:foreground ,tennis-orange :inherit ruler-mode-default))))
   `(ruler-mode-fringes        ((t (:foreground ,tennis-green :inherit ruler-mode-default))))
   `(ruler-mode-goal-column    ((t (:foreground ,tennis-cyan :inherit ruler-mode-default))))
   `(ruler-mode-margins        ((t (:inherit ruler-mode-default))))
   `(ruler-mode-tab-stop       ((t (:foreground ,tennis-mono-3 :inherit ruler-mode-default))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face    ((t (:foreground ,tennis-red))))
   `(undo-tree-visualizer-register-face   ((t (:foreground ,tennis-orange))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,tennis-cyan))))

   ;; tab-bar-mode
   `(tab-bar-tab-inactive ((t (:background ,tennis-bg-hl :foreground ,tennis-fg))))
   `(tab-bar-tab          ((t (:background ,tennis-bg :foreground ,tennis-purple))))
   `(tab-bar              ((t (:background ,tennis-bg-hl))))

   ;; all-the-icons
   `(all-the-icons-purple    ((t (:foreground ,tennis-purple))))
   `(all-the-icons-yellow    ((t (:foreground ,tennis-yellow))))

   ;; dashboard
   `(dashboard-heading        ((t (:foreground ,tennis-green))))
   `(dashboard-items-face     ((t (:bold ,tennis-green))))

   ;; Language Customizations ----------------------------------------------------------------------
   ;; these laguage customizations are seperate from certain larger lagu

   ;; markdown
   `(markdown-header-face-1            ((t (:foreground ,tennis-red :weight bold))))
   `(markdown-header-face-2            ((t (:foreground ,tennis-orange :weight bold))))
   `(markdown-link-face                ((t (:foreground ,tennis-purple ))))
   `(markdown-url-face                 ((t (:foreground ,tennis-blue :underline t))))
   `(markdown-plain-url-face           ((t (:foreground ,tennis-blue))))
   `(markdown-header-delimiter-face    ((t (:foreground ,tennis-silver))))
   `(markdown-language-keyword-face    ((t (:foreground ,tennis-green))))
   `(markdown-markup-face              ((t (:foreground ,tennis-silver))))
   `(markdown-pre-face                 ((t (:foreground ,tennis-green))))
   `(markdown-metadata-key-face        ((t (:foreground ,tennis-green))))

   ;; org-mode
   `(org-date                  ((t (:foreground ,tennis-cyan))))
   `(org-document-info         ((t (:foreground ,tennis-mono-3))))
   `(org-document-info-keyword ((t (:inherit org-meta-line :underline t))))
   `(org-document-title        ((t (:weight bold))))
   `(org-footnote              ((t (:foreground ,tennis-cyan))))
   `(org-sexp-date             ((t (:foreground ,tennis-cyan))))
   `(org-table                 ((t (:foreground ,tennis-blue))))
   `(org-drawer                ((t (:foreground ,tennis-blue))))
   `(org-headline-done         ((t (:foreground ,tennis-purple))))
   ;; `(org-block                 ((t (:background ,tennis-bg-hl :foreground ,tennis-silver :extend t))))
   ;; `(org-block-begin-line      ((t (:background ,tennis-bg-hl :foreground ,tennis-silver :extend t))))
   ;; `(org-block-end-line        ((t (:background ,tennis-bg-hl :foreground ,tennis-silver :extend t))))
   `(org-level-1               ((t (:foreground ,tennis-green))))
   `(org-level-2               ((t (:foreground ,tennis-red))))
   `(org-level-3               ((t (:foreground ,tennis-purple))))
   `(org-level-4               ((t (:foreground ,tennis-orange))))
   `(org-level-6               ((t (:foreground ,tennis-blue))))
   `(org-level-7               ((t (:foreground ,tennis-silver))))
   `(org-level-8               ((t (:foreground ,tennis-cyan))))
   `(org-cite                  ((t (:foreground ,tennis-blue :weight bold))))
   `(org-cite-key              ((t (:foreground ,tennis-green :weight bold))))
   `(org-hide                  ((t (:foreground ,tennis-bg))))

   ;; latex-mode
   `(font-latex-sectioning-0-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-sectioning-1-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-sectioning-2-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-sectioning-3-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-sectioning-4-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-sectioning-5-face        ((t (:foreground ,tennis-blue :height 1.0))))
   `(font-latex-bold-face                ((t (:foreground ,tennis-green :weight bold))))
   `(font-latex-italic-face              ((t (:foreground ,tennis-green :slant italic))))
   `(font-latex-warning-face             ((t (:foreground ,tennis-red))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,tennis-cyan))))
   `(font-latex-script-char-face         ((t (:foreground ,tennis-gray))))

   ;; sh-mode
   `(sh-heredoc ((t (:inherit font-lock-string-face :slant italic))))

   ;; js2-mode
   `(js2-error             ((t (:underline (:color ,tennis-red :style wave)))))
   `(js2-external-variable ((t (:foreground ,tennis-cyan))))
   `(js2-warning           ((t (:underline (:color ,tennis-orange :style wave)))))
   `(js2-function-call     ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param    ((t (:foreground ,tennis-mono-1))))
   `(js2-jsdoc-tag         ((t (:foreground ,tennis-purple))))
   `(js2-jsdoc-type        ((t (:foreground ,tennis-yellow))))
   `(js2-jsdoc-value       ((t (:foreground ,tennis-red))))
   `(js2-object-property   ((t (:foreground ,tennis-red))))

   ;; whitespace-mode
   `(whitespace-big-indent       ((t (:foreground ,tennis-border))))
   `(whitespace-empty            ((t (:foreground ,tennis-border))))
   `(whitespace-hspace           ((t (:foreground ,tennis-border))))
   `(whitespace-indentation      ((t (:foreground ,tennis-border))))
   `(whitespace-line             ((t (:background ,tennis-border))))
   `(whitespace-newline          ((t (:foreground ,tennis-border))))
   `(whitespace-space            ((t (:foreground ,tennis-border))))
   `(whitespace-space-after-tab  ((t (:foreground ,tennis-border))))
   `(whitespace-space-before-tab ((t (:foreground ,tennis-border))))
   `(whitespace-tab              ((t (:foreground ,tennis-border))))
   `(whitespace-trailing         ((t (:foreground ,tennis-silver))))
   ))

(tennis-with-color-variables
  (custom-theme-set-variables
   'tennis
   ;; fill-column-indicator
   `(fci-rule-color ,tennis-gray)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `tennis-yellow' |
   ;; | J         | `tennis-blue'     |
   ;; | L         | `tennis-orange' |
   ;; | Z         | `tennis-red'    |
   ;; | S         | `tennis-green'    |
   ;; | T         | `tennis-purple'   |
   ;; | I         | `tennis-cyan'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,tennis-black ,tennis-red ,tennis-green ,tennis-yellow
                  ,tennis-blue ,tennis-purple ,tennis-cyan ,tennis-fg])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
          (file-name-as-directory
           (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'tennis)
(provide 'tennis-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; tennis-theme.el ends here
