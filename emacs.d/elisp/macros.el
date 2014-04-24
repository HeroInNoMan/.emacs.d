;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makes a JIRA ticket number into an org hyperlink to this ticket.
(fset 'linkify-jira-ticket
      [?\C-s ?W ?D ?I ?A ?G ?- ?\C-m ?\C-c ?e ?e ?\M-w ?\[ ?\[ ?h ?t ?t ?p ?s ?: ?/ ?/ ?j ?i ?r ?a ?. ?v ?s ?c ?t ?. ?f ?r ?/ ?j ?i ?r ?a ?/ ?b ?r ?o ?w ?s ?e ?/ ?\C-y ?\] ?\[ ?\M-f ?\M-f ?\] ?\] ?  backspace])
;; transforms code into concatenated strings to be inserted in java
;; code (as a string). "s are escaped so java doesn’t misinterpret
;; them.
(fset 'stringify-code-for-java
      [?\M-x ?t ?e ?x ?t ?- ?m ?o ?d ?e return ?\C-c ?i ?\C-c ?h ?$ backspace ?\" return ?\\ ?\" return ?\M-< ?\C-c ?j ?^ return ?\" return ?\M-< ?\C-c ?j ?$ return ?\" ?  ?+ ?  ?/ ?/ return backspace backspace backspace backspace backspace])
;; for vimtutor
(fset 'vim-tutor-next-lesson
      "\C-s~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\C-m\C-l\C-l\C-a\C-n\C-n\C-n\C-n")
