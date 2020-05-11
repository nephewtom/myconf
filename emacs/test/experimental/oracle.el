
(let ((oracle-home (shell-command-to-string ". ~/.profile; echo $JAVA_HOME")))
  (if oracle-home
      (setenv "ORACLE_HOME" oracle-home))
  (setenv "PATH" (concat (getenv "PATH")
                         (format "%s/%s" oracle-home "instantclient_12_1")))
  (add-to-list 'exec-path (format "%s/%s" oracle-home "instantclient_12_1"))
)
