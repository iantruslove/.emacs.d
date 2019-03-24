(defhydra hydra-mark (:hint nil)
  "
^Structure^      ^Pairs^              ^Misc^
^^^^^^^^-------------------------------------------
_=_: region      _P_: inside pairs    _u_: url
_-_: contract    _p_: outside pairs   _m_: email
_d_: defun       _Q_: inside quotes   _s_: symbol
_c_: comment     _q_: outside quotes
_._: sentence
_h_: paragraph
"
  ("=" er/expand-region)
  ("-" er/contract-region)
  ("P" er/mark-inside-pairs)
  ("Q" er/mark-inside-quotes)
  ("p" er/mark-outside-pairs)
  ("q" er/mark-outside-quotes)
  ("d" er/mark-defun)
  ("c" er/mark-comment)
  ("." er/mark-text-sentence)
  ("h" er/mark-text-paragraph)
  ("w" er/mark-word)
  ("u" er/mark-url)
  ("m" er/mark-email)
  ("s" er/mark-symbol))
