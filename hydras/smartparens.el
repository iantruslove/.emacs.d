;; From https://gist.githubusercontent.com/ptrv/7e27e0e18ae37d3f80aff7b9da749321/raw/0685c9b9b35b75886eb372b03f9fde601094d089/smartparens-hydra.el

(defhydra ian/smartparens (:hint nil)
  "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
  ("q" nil)
  ;; Wrapping
  ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
  ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
  ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
  ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
  ;; Navigation
  ("f" sp-forward-sexp )
  ("b" sp-backward-sexp)
  ("u" sp-backward-up-sexp)
  ("d" sp-down-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ;; Kill/copy
  ("w" sp-copy-sexp)
  ("k" sp-kill-sexp)
  ;; Misc
  ("t" sp-transpose-sexp)
  ("j" sp-join-sexp)
  ("s" sp-split-sexp)
  ("c" sp-convolute-sexp)
  ("i" sp-indent-defun)
  ;; Depth changing
  ("R" sp-splice-sexp)
  ("r" sp-splice-sexp-killing-around)
  ("<up>" sp-splice-sexp-killing-backward)
  ("<down>" sp-splice-sexp-killing-forward)
  ;; Barfing/slurping
  ("<right>" sp-forward-slurp-sexp)
  ("<left>" sp-forward-barf-sexp)
  ("C-<left>" sp-backward-barf-sexp)
  ("C-<right>" sp-backward-slurp-sexp))
