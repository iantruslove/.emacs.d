;; Org
(use-package org
  :ensure t
  :pin melpa)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'org-store-link)
            (local-set-key (kbd "C-c C-l") 'org-insert-link)
            ;; Always use org-indent-mode
            (org-indent-mode t)))

(setq org-directory "~/org/"
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-agenda-files '("~/org/")
      org-log-into-drawer 't
      org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

(defvar organizer (concat org-directory "organizer.org"))
(defvar cambium-organizer (concat org-directory "cambium/cambium-organizer.org"))
(setq journal (concat org-directory "journal.org"))
(defvar lg-journal (concat org-directory "lg/lg-journal.org"))

(setq org-default-notes-file 'organizer)

;; Speed commands
(setq org-use-speed-commands t)
(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Quick shortcuts to open important files:
(global-set-key (kbd "C-c o")
                (lambda ()
                  (interactive)
                  (find-file organizer)))
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (find-file lg-journal)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(i!)" "WAIT(w@/!)"
                  "|"
                  "DONE(d!)" "CANCELED(c@)")))

(setq org-tag-alist '(;; GTD-ish Contexts:
                      (:startgroup . nil)
                      ("@lg"       . ?L)
                      ("@cambium"  . ?G)
                      ("@home"     . ?H)
                      ;; ("@nerding"  . ?N)
                      ;; ("@errands"  . ?E)
                      ;; ("@coding"   . ?C)
                      ;; ("@phone"    . ?P)
                      ;; ("@reading"  . ?R)
                      (:endgroup   . nil)

                      ;; ;; Structural
                      ("project" . ?p)  ;; identifies a TODO item as a project
                      ("inbox"   . ?i)
                      ("ARCHIVE" . ?A)

                      ;; ;; Others:
                      ("blog"    . ?b)
                      ("idea"    . ?d)
                      ("meeting" . ?m)
                      ;; ("retro"   . ?o)
                      ("REFILE"  . ?r)
                      ;; ("sm"      . ?s)
                      ("talk"    . ?t)))

;; Project tags shouldn't automatically propagate down to subtasks
(setq org-tags-exclude-from-inheritance '("project" "inbox"))

;; Don't show days in rolled-up times
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Size: %^{effort|0:05|0:15|0:30|1:00|2:00|4:00}
:Captured: %U
:END:
%?

%i
" "Basic task data")

(setq org-capture-templates
      `(
        ;; ("T" "Detailed Task" entry
        ;;  (file+headline organizer "Inbox")
        ;;  ,my/org-basic-task-template)
        ("T" "Quick organizer TODO" entry
         (file+headline organizer "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :immediate-finish t)

        ("t" "Quick LG journal TODO" entry
         (file+datetree lg-journal)
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ("c" "Quick Cambium TODO" entry
         (file+headline cambium-organizer "Cambium Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:Captured: %U\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ("j" "Quick LG Journal Entry" entry
         (file+datetree lg-journal)
         "* %^{Title}\n:PROPERTIES:\n:Captured: [%<%H:%M>]\n:END:\n"
         :empty-lines 1
         :immediate-finish t)

        ;; ("J" "Context-specific Journal Entry"
        ;;  entry (file+datetree journal)
        ;;  "* %?\n:PROPERTIES:\n:Captured:[%<%H:%M>]\n:END:\n\n  %i\n\n  From: %a"
        ;;  :empty-lines 1)
        ))

(defun my/org-match-at-point-p (match &optional todo-only)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'.
If the optional argument TODO-ONLY is non-nil, do not declare a
match unless headline at point is a todo item."
  (let ((todo      (org-get-todo-state))
        (tags-list (org-get-tags-at)))
    (eval (cdr (org-make-tags-matcher match)))))

(defun my/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.
Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.  Intended for
use with `org-agenda-skip-function', where this will skip exactly
those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))

(defun my/org-agenda-skip-with-match (match)
  "Skip current headline if it matches MATCH.
Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.
Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) next-headline nil))))

(setq org-agenda-custom-commands
      '(("A" "Agenda and todos"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-deadline-warning-days 7)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-agenda-repeating-timestamp-show-all nil)))

          (tags-todo "DEADLINE<\"<+0d>\""
                     ((org-agenda-overriding-header "OVERDUE")))
          (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                     ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
          (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                     ((org-agenda-overriding-header "HIGH PRIORITY")))
          (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                     ((org-agenda-overriding-header "FLAGGED")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-when-regexp-matches))
                      (org-agenda-skip-regexp "\\[#A\\]")))
          (tags-todo "strategy"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "+cambium-strategy"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "horus"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "@lg"
                     ((org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "@home")
          (tags-todo "@nerding")
          (tags-todo "@errands")
          (tags-todo "@coding")
          (tags-todo "@phone")
          (tags-todo "@reading")))

        ("c" "Cambium"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-deadline-warning-days 14)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-agenda-repeating-timestamp-show-all nil)))
          (tags-todo "PRIORITY={A}"
                     ((org-agenda-overriding-header "HIGH PRIORITY")))
          (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                     ((org-agenda-overriding-header "FLAGGED")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-when-regexp-matches))
                      (org-agenda-skip-regexp "\\[#A\\]")))
          (tags-todo "strategy"
                     ((org-agenda-overriding-header "STRATEGY")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "website"
                     ((org-agenda-overriding-header "WEBSITE")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-when-regexp-matches))
                      (org-agenda-skip-regexp "\\[#C\\]")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "project"
                     ((org-agenda-overriding-header "PROJECTS")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "sos"
                     ((org-agenda-overriding-header "SOS")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "blue_moon"
                     ((org-agenda-overriding-header "BLUE MOON")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "+cambium-strategy-website-project-sos-blue_moon"
                     ((org-agenda-overriding-header "OTHER CAMBIUM TODOS")
                      (org-agenda-sorting-strategy
                       '(scheduled-down priority-down todo-state-up))))
          (tags-todo "+cambium+PRIORITY=\"C\""
                     ((org-agenda-overriding-header "LOW PRIORITY")
                      (org-agenda-sorting-strategy '(priority-down))))))

        ("L" "Long agenda"
         ((agenda ""
                  ((org-agenda-ndays 60)
                   (org-deadline-warning-days 7)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-agenda-time-grid nil)
                   (org-agenda-repeating-timestamp-show-all nil)
                   (org-agenda-show-log t)
                   ;; only show days with something:
                   (org-agenda-show-all-dates nil)))))

        ("K" "tasK summary (refile, in progress, todo)"
         ((tags-todo "REFILE"
                     ((org-agenda-overriding-header "Tasks to refile")
                      (org-agenda-sorting-strategy '(priority-down))))
          (todo "IN PROGRESS"
                ((org-agenda-overriding-header "Tasks In Progress")
                 (org-agenda-sorting-strategy '(priority-down))))
          (todo "TODO"
                ((org-agenda-overriding-header "Tasks TODO")
                 (org-agenda-skip-function '(my/org-agenda-skip-with-match ":REFILE:"))
                 (org-agenda-sorting-strategy '(priority-down))))))

        ("D" "toDay"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-show-log t)
                   (org-agenda-time-grid nil)
                   (org-agenda-log-mode-items '(state))
                   (org-agenda-sorting-strategy
                    (quote ((agenda time-up priority-down tag-up))))
                   (org-deadline-warning-days 0)))
          (todo "IN PROGRESS"
                ((org-agenda-overriding-header "Tasks In Progress")
                 (org-agenda-sorting-strategy '(priority-down))))))

        ("td" "Daily Timesheet"
         ((agenda ""))
         ((org-agenda-log-mode-items '(clock closed))
          (org-agenda-overriding-header "DAILY TIMESHEET")
          (org-agenda-show-log 'clockcheck)
          (org-agenda-span 'day)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("tw" "Weekly Timesheet"
         ((agenda ""))
         (
          ;; (org-agenda-format-date "")
          (org-agenda-overriding-header "WEEKLY TIMESHEET")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
          (org-agenda-span 'week)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("tm" "Monthly Timesheet"
         ((agenda ""))
         (
          ;; (org-agenda-format-date "")
          (org-agenda-overriding-header "MONTHLY TIMESHEET")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
          (org-agenda-span 'month)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-time-grid nil)))

        ("S" "Scratch"
         ;;((tags "project+CATEGORY=\"elephants\""))
         ((tags "retro")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel and friends

(require 'ob-clojure)
(use-package es-mode
  :ensure t)

(use-package ob-http
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)
   (clojure . t)
   (sh . t)
   (elasticsearch . t)))

(setq org-babel-sh-command "bash"
        org-babel-clojure-backend 'cider
        org-confirm-babel-evaluate nil
        org-babel-default-header-args:clojure '((:results . "value verbatim pp"))
        org-babel-default-header-args:sh '((:results . "verbatim drawer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic block encryption: encrypt any heading contents tagged with ":crypt:".

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove all source block results
;; from https://www.wisdomandwonder.com/article/10597/remove-every-source-block-results

(defconst help/org-special-pre "^\s*#[+]")

(defun help/org-2every-src-block (fn)
  "Visit every Source-Block and evaluate `FN'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (concat help/org-special-pre "BEGIN_SRC") nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun org-babel-remove-every-results-block ()
  (interactive)
  (help/org-2every-src-block (lambda (_)
                               (funcall 'org-babel-remove-result))))

(define-key org-mode-map (kbd "M-]")
  'org-babel-remove-every-results-block)
