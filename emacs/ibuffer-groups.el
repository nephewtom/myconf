;; This file is to configure IBuffer as I like "on the fly"

(defun my-ibuffer-saved-groups ()
  (setq ibuffer-saved-filter-groups
        '(("home"
           ("myconf" (or (filename . "myconf")
                         (filename . "emacs-config")))

           ;; SMIP related
           ("certificates" (filename . "iop-certificates"))
           ("grizzly-poc" (filename . "grizzly-poc"))
           ("INC000004157871" (filename . "INC000004157871"))
           ("Ansible TEF" (filename . "ansible/tef"))
           ("Ansible IOP" (filename . "ansible/sm2m"))
           ("h2" (filename . "vagrant/h2"))
           ("mc-core" (filename . "mc-core"))
           ("ng-core" (filename . "ng-core"))
           ("Jenkins" (filename . "jenkins"))
           ("SMIP" (filename . "smip"))

           ;; General
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ;; Coding
           ("milton" (filename . "milton"))
           ("playground" (filename . "playground"))
           ("/usr/include/" (filename . "/usr/include/"))
           ("SDL" (filename . "tomas/SDL/"))
           ("raylib" (filename . "raylib"))
           )
          
          ("default")
          ))
  (ibuffer-switch-to-saved-filter-groups "home" ))


;;(add-hook 'ibuffer-mode-hook 'my-ibuffer-saved-groups)
