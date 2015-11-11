;; -*- coding: utf-8 -*-
;; starcraft.el -- track user APM (actions per minute) while using Emacs
;;
;; Copyright 2010 by Michael Steder
;; Author: Michael Steder (steder@gmail.com)
;; Created: 2010
;; Version: 1.0
;; Keywords: Actions per minute
;;
;; Inspired by a love of Starcraft and someones offhand comment
;; that their APM at work was significantly higher than their
;; Starcraft APM I started to wonder how I would determine if that
;; were actually true for myself.  Being an Emacs user I felt
;; that the obvious way to answer this question was to develop
;; an Emacs mode that tracks emacs commands/actions.
;;
;; This is also an exercise in learning how to extend Emacs with
;; a new minor mode which may be more useful.
;;
;; Usage:
;;  Add this file to a directory in emacs load path and add the following
;;  to your ~/init.el or ~/.emacs file:
;;
;;  (require 'starcraft)
;;  (starcraft-mode t)
;;

;; Defining a minor mode:

(defgroup starcraft nil
  "Customization group for starcraft mode"
  :package-version '(starcraft . "1.0")
  :group 'local
  :prefix "starcraft")

(defun starcraft-mode-activate ()
  (add-hook 'pre-command-hook 'starcraft-count-action)
  (starcraft-stopwatch-start)
  (starcraft-mode-idle-timer-start)
  )

(defun starcraft-mode-deactivate ()
  (remove-hook 'pre-command-hook 'starcraft-count-action)
  (starcraft-mode-idle-timer-stop)
  )

(define-minor-mode starcraft-mode
  "Starcraft mode records actions (Emacs commands) per minute"
  :global t
  :init-value nil
  :lighter " sc"
  :keymap nil
  :group 'starcraft

  (if starcraft-mode
      (starcraft-mode-activate)
    (starcraft-mode-deactivate)
    )
  )

;; Mode hook which counts actions
(defvar starcraft-actions 0)

(defun starcraft-count-action ()
  "Updates the counter and restarts the stopwatch if necessary (the stopwatch can be stopped by going idle)"
  (let ((command real-last-command))
    (when command
      (setq starcraft-actions (+ starcraft-actions 1))
      (when (not starcraft-stopwatch-running)
        (starcraft-stopwatch-start))
      )
    )
  )

;; stopwatch stuff to track the total non-idle time
(defvar starcraft-start-time 0.0)
(defvar starcraft-elapsed-time 0.0)
(defvar starcraft-stopwatch-running nil)

(defun starcraft-stopwatch-stop ()
  "Updates the elapsed time and marks the stopwatch as no longer running"
  (setq starcraft-elapsed-time (starcraft-stopwatch-read))
  (setq starcraft-stopwatch-running nil)
  )

(defun starcraft-stopwatch-reset ()
  "Resets the stopwatch to a time of zero"
  (setq starcraft-elapsed-time 0.0)
  (setq starcraft-start-time (float-time))
  )

(defun starcraft-stopwatch-start ()
  "Updates the start time and marks the stopwatch as running"
  (setq starcraft-stopwatch-running t)
  (setq starcraft-start-time (float-time))
  )

(defun starcraft-stopwatch-read ()
  "Returns the current elapsed time"
  (if starcraft-stopwatch-running
      (setq xtime (+ starcraft-elapsed-time (- (float-time) starcraft-start-time)))
    starcraft-elapsed-time))

;; idle hooks:
"Currently apm is calculated over the full duration of the emacs session.  Idle time
is included and will cause your APM to drop.  This may be consistent with the starcraft
meaning of APM and it may not.

I'm thinking that when emacs goes idle that I'll report the current APM and stop the stopwatch.
This way your APM doesn't drop drastically when you go to lunch and leave emacs running.  The bottomline
is that while in starcraft it makes sense to calculate APM over the course of the entire game
that's because they can assume that you are actively playing the whole time the game is running
and is not paused.

Obviously the next step is a webservice that allows you to record your emacs apm and compete
with other emacs users... ;-)

"
(defvar starcraft-mode-idle-timer nil)
(defvar starcraft-mode-idle-threshold 10)

;; idle callback
(defun starcraft-mode-idle-callback ()
  ""
  (starcraft-stopwatch-stop)
  (starcraft-apm)
  )

(defun starcraft-mode-idle-timer-start ()
  ""
  (when (timerp starcraft-mode-idle-timer)
    (cancel-timer starcraft-mode-idle-timer))
  (setq starcraft-mode-idle-timer (run-with-idle-timer starcraft-mode-idle-threshold  t 'starcraft-mode-idle-callback))
  )

(defun starcraft-mode-idle-timer-stop ()
  ""
  (when (timerp starcraft-mode-idle-timer)
    (cancel-timer starcraft-mode-idle-timer))
  )

(defun starcraft-apm ()
  "The actual apm calculation"
  (interactive)
  (message
   (let ((xtime (starcraft-stopwatch-read)))
        (format "APM: %6.2f (%d in %6.2f seconds)"
                (/ starcraft-actions xtime) starcraft-actions xtime)
        )
   )
  )

(provide 'starcraft)