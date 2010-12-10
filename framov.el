;;; framov.el --- minor mode and any commands for Emacsen on X-Window System

;; Copyright (C) 2003 lieutar Ohnishi

;; Maintainer: lieutar Ohnishi <lieutar@members10.tsukaeru.net>
;; Keywords: frame
;; Created: Thu Sep 18 14:19:30  2003

(defconst framov-version "0.1" "framov version number.")

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;;----------------------------------------------------------------------

;;; code
(require 'easy-mmode)
(require 'cl)

(defvar framov-step      0
 "フレームの移動距離のレベル．この値は指数であり，
移動距離は 2^framov-step となる．")

(defvar framov-step-max 10
  "framov-step の上限．Emacs Lispの整数値は
27ビットの符号付きのものなので，27以上での
動作はきっと変．")

(defvar framov-x-screen-margin '(-2 -4 48 -4)
  "ウインドウシステムのスクリーンで,framov によって動かさない領域のサイズ．
top right bottom left の順番にその値を並べたリストで，
値にマイナスを与える事によって，スクリーンからはみだすことができる．

各 emacsen や，ウインドウマネージャを含む環境によって，座標の評価が結構
まちまちなので，適当に調整すると良し")

(defconst framov-mode-map
  (let ((map '(keymap (t . undefined))))
     (define-key map [?q]    'framov-mode-exit)

     (define-key map [?h]    'framov-L)
     (define-key map [?j]    'framov-D)
     (define-key map [?k]    'framov-U)
     (define-key map [?l]    'framov-R)
     (define-key map [?y]    'framov-UL)
     (define-key map [?u]    'framov-UR)
     (define-key map [?b]    'framov-DL)
     (define-key map [?n]    'framov-DR)

     (define-key map [?\C-h] 'framov-step-L)
     (define-key map [?\C-j] 'framov-step-D)
     (define-key map [?\C-k] 'framov-step-U)
     (define-key map [?\C-l] 'framov-step-R)
     (define-key map [?\C-y] 'framov-step-UL)
     (define-key map [?\C-u] 'framov-step-UR)
     (define-key map [?\C-b] 'framov-step-DL)
     (define-key map [?\C-n] 'framov-step-DR)

     (define-key map [?\M-h] 'framov-LE)
     (define-key map [?\M-j] 'framov-DE)
     (define-key map [?\M-k] 'framov-UE)
     (define-key map [?\M-l] 'framov-RE)

     (define-key map [?H]    'framov-resize-SH)
     (define-key map [?J]    'framov-resize-EV)
     (define-key map [?K]    'framov-resize-SV)
     (define-key map [?L]    'framov-resize-EH)
     (define-key map [?Y]    'framov-resize-SVSH)
     (define-key map [?U]    'framov-resize-SVEH)
     (define-key map [?B]    'framov-resize-EVSH)
     (define-key map [?N]    'framov-resize-EVEH)

     (define-key map [\up]   'framov-step-inc)
     (define-key map [\down] 'framov-step-dec)

     (define-key map [?1]    'delete-other-windows)
     (define-key map [?2]    'split-window-vertically)
     (define-key map [?3]    'split-window-horizontally)
     (define-key map [?0]    'delete-window)

     (define-key map [?5 ?2] '(lambda ()(interactive)
                                (select-frame (make-frame))))
     (define-key map [?5 ?o] 'other-frame)
     (define-key map [?5 ?0] 'delete-frame)
     map)
  "framov-mode のキーマップ")



;;;;


(defun framov-display-status (&optional frame)
  "フレーム位置のステータスを表示する"
  (if (null frame) (setq frame (selected-frame)))
  (message (format "geom: %dx%d+%d-%d  step: %dpx"
		   (frame-width           frame)
		   (frame-height          frame)
		   (framov-get-frame-top  frame)
		   (framov-get-frame-left frame)
		   (framov-step))))



;;;----------------------------------------------------------------------
;;;
;;; 移動距離に関する関数 / コマンド
;;;

(defun framov-step ()
  "framov-step の値からフレームの移動距離を算出する"
  (expt 2 framov-step))

(defun framov-step-inc ()
  "framov-step の値をインクリメントする"
  (interactive)
  (setq framov-step
	(+ framov-step
	   (if (>= framov-step framov-step-max) 0 1)))
  (framov-display-status))

(defun framov-step-dec ()
  "framov-step の値をデクリメントする"
  (interactive)
  (setq framov-step
	(- framov-step
	   (if (>= 0 framov-step) 0 1)))
  (framov-display-status))





;;;----------------------------------------------------------------------
;;;
;;; 物理座標上における位置
;;;

(defun framov-get-frame-top (&optional frame)
  "フレームの実際のスクリーンにおける位置のうち，上端からのピクセル数を返す"
  (if frame nil (setq frame (selected-frame)))
  (let ((R (cdr (assoc 'top (frame-parameters  frame)))))
    (if (listp R) (cadr R) R)))


(defun framov-get-frame-left (&optional frame)
  "フレームの実際のスクリーンにおける位置のうち，左端からのピクセル数を返す"
  (if frame nil (setq frame (selected-frame)))
  (let ((R (cdr (assoc 'left (frame-parameters frame)))))
    (if (listp R) (cadr R) R)))





;;;----------------------------------------------------------------------
;;;
;;; ワールド座標系に関する関数
;;;

(defsubst framov-x-screen-margin-top   ()(car    framov-x-screen-margin))
(defsubst framov-x-screen-margin-right ()(cadr   framov-x-screen-margin))
(defsubst framov-x-screen-margin-bottom()(caddr  framov-x-screen-margin))
(defsubst framov-x-screen-margin-left  ()(cadddr framov-x-screen-margin))

(defun framov-world-size (&optional frame)
  "framovでのワールド座標系の大きさを (width . height) の形で返す．
framov ではフレーム毎に，ワールド座標系を持ち，その上での座標を持っている．
これは，フレームの左上端が移動できる領域だけの大きさを持ち，
フレームはこの範囲を越えて移動する事が出来ない．"
  (cons (framov-world-width  frame)
	(framov-world-height frame)))

(defun framov-world-width (&optional frame)
  "framov でのワールド座標系の幅を返す．"
  (if frame nil (setq frame (selected-frame)))
  (- (x-display-pixel-width)
     (frame-pixel-width  frame)
     (framov-x-screen-margin-left)
     (framov-x-screen-margin-right)))

(defun framov-world-height (&optional frame)
  "framov でのワールド座標系の高さを返す．"
  (if frame nil (setq frame (selected-frame))) 
  (- (x-display-pixel-height)
     (frame-pixel-height frame)
     (framov-x-screen-margin-top)
     (framov-x-screen-margin-bottom)))

(defun framov-world-left (&optional frame)
  "framov でのワールド座標系上のフレームの座標を返す"
  (if frame nil (setq frame (selected-frame)))
  (- (framov-get-frame-left frame)
     (framov-x-screen-margin-left)))

(defun framov-world-top (&optional frame)
  "framov でのワールド座標系上のフレームの座標を返す"
  (if frame nil (setq frame (selected-frame)))
  (- (framov-get-frame-top  frame)
     (framov-x-screen-margin-top)))


(defun framov-world-physical-left (frame left)
  "与えられたフレームと， その framov ワールド座標系での左端からの距離を，
物理的なそれに変換する．
ワールド座標系からはみ出すものに関しては，その値が修正される．"
  (let ((world-width (framov-world-width frame)))
    (+ (framov-x-screen-margin-left)
       (cond ((>            0 left)                0)
	     ((<= world-width left)(-  world-width 1))
	     (t                                 left)))))

(defun framov-world-physical-top  (frame top)
"与えられたフレームと， その framov ワールド座標系での上端からの距離を，
物理的なそれに変換する
ワールド座標系からはみ出すものに関しては，その値が修正される．"
  (let ((world-height (framov-world-height frame)))
    (+ (framov-x-screen-margin-top)
       (cond ((>             0 top)                0)
	     ((<= world-height top)(- world-height 1))
	     (t                                  top)))))



;;;----------------------------------------------------------------------
;;;
;;; フレームの移動
;;;
(defun framov-abs-move (frame left top)
  "フレームを絶対移動する．引数の left と top は framov ワールド座標系上の
上端および左端からのピクセル数"
  (set-frame-position frame
		      (framov-world-physical-left frame left)
		      (framov-world-physical-top  frame top))
  (framov-display-status frame))


(defun framov-move (frame left+ top+)
  "フレームを相対移動する"
  (framov-abs-move frame
		   (+ (framov-world-left frame) left+)
		   (+ (framov-world-top  frame) top+ )))


;; 一気に移動
(defun framov-RE (frame)
  "フレームの位置を右端へ"
  (interactive (list (selected-frame)))
  (framov-abs-move frame
		   (- (framov-world-width frame) 1)
		   (framov-world-top frame)))
(defun framov-LE (frame)
  "フレームの位置を左端へ"
  (interactive (list (selected-frame)))
  (framov-abs-move frame
		   0
		   (framov-world-top frame)))
(defun framov-UE (frame)
  "フレームの位置を上端へ"
  (interactive (list (selected-frame)))
  (framov-abs-move frame
		   (framov-world-left frame)
		   0))
(defun framov-DE (frame)
  "フレームの位置を下端へ"
  (interactive (list (selected-frame)))
  (framov-abs-move frame
		   (framov-world-left frame)
		   (- (framov-world-height frame) 1)))


;; フレーム単位移動
(defun framov-step-U (frame)
  "フレームの位置を上へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (*  0 (frame-pixel-width  frame))
	       (* -1 (frame-pixel-height frame))))
(defun framov-step-UR (frame)
  "フレームの位置を右上へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (*  1 (frame-pixel-width  frame))
	       (* -1 (frame-pixel-height frame))))
(defun framov-step-R (frame)
  "フレームの位置を右へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (*  1 (frame-pixel-width  frame))
	       (*  0 (frame-pixel-height frame))))
(defun framov-step-DR (frame)
  "フレームの位置を右下へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (*  1 (frame-pixel-width  frame))
	       (*  1 (frame-pixel-height frame))))
(defun framov-step-D (frame)
  "フレームの位置を下へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (*  0 (frame-pixel-width  frame))
	       (*  1 (frame-pixel-height frame))))
(defun framov-step-DL (frame)
  "フレームの位置を左下へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (* -1 (frame-pixel-width  frame))
	       (*  1 (frame-pixel-height frame))))
(defun framov-step-L (frame)
  "フレームの位置を左へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (* -1 (frame-pixel-width  frame))
	       (*  0 (frame-pixel-height frame))))
(defun framov-step-UL (frame)
  "フレームの位置を左上へ"
  (interactive (list (selected-frame)))
  (framov-move frame
	       (* -1 (frame-pixel-width  frame))
	       (* -1 (frame-pixel-height frame))))



;; インクリメンタル移動
(defun framov-U (frame)
  "フレームの位置を上へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* 0 (framov-step)) (* -1 (framov-step))))
(defun framov-UR (frame)
  "フレームの位置を右上へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* 1 (framov-step)) (* -1 (framov-step))))
(defun framov-R (frame)
  "フレームの位置を右へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* 1 (framov-step)) (* 0 (framov-step))))
(defun framov-DR (frame)
  "フレームの位置を右下へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* 1 (framov-step)) (* 1 (framov-step))))
(defun framov-D (frame)
  "フレームの位置を下へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* 0 (framov-step)) (* 1 (framov-step))))
(defun framov-DL (frame)
  "フレームの位置を左下へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* -1 (framov-step)) (* 1 (framov-step))))
(defun framov-L (frame)
  "フレームの位置を左へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* -1 (framov-step)) (* 0 (framov-step))))
(defun framov-UL (frame)
  "フレームの位置を左上へ"
  (interactive (list (selected-frame)))
  (framov-move frame (* -1 (framov-step)) (* -1 (framov-step))))






;;;----------------------------------------------------------------------
;;;
;;; リサイズ関連
;;;
(defun framov-frame-resize (frame w+ h+)
  (set-frame-size frame
		  (+ (frame-width  frame)   w+)
		  (+ (frame-height frame) h+))
  (framov-display-status))
(defun framov-resize-SV (frame)
  "フレームの縦方向を縮小"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame 0 -1))
(defun framov-resize-SH (frame)
  "フレームの横方向を縮小"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame -1 0))
(defun framov-resize-EV (frame)
  "フレームの縦方向を拡大"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame 0 1))
(defun framov-resize-EH (frame)
  "フレームの横方向を縮小"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame 1 0))
(defun framov-resize-SVSH (frame)
  "フレームの縦方向を縮小，横方向を縮小"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame -1 -1))
(defun framov-resize-SVEH (frame)
  "フレームの縦方向を縮小，横方向を拡大"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame 1 -1))
(defun framov-resize-EVSH (frame)
  "フレームの縦方向を拡大，横方向を縮小"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame -1 1))
(defun framov-resize-EVEH (frame)
  "フレームの縦方向を拡大，横方向を拡大"
  (interactive (list (selected-frame)))
  (framov-frame-resize frame 1 1))

(defun framov-maximize-horizontal (frame &optional n)
  (interactive (list (selected-frame))))
(defun framov-maximize-vertical   (frame &optional n)
  (interactive (list (selected-frame))))
(defun framov-maximize (frame &optional n)
  (interactive (list (selected-frame)))
  (framov-maximize-horizontal n)
  (framov-minimize-horizontal n))




;;; framov から抜ける
(defun framov-mode-exit () (interactive)(global-framov-mode))


;;;----------------------------------------------------------------------
;;;
;;; 各種変数等の初期化
;;;

(easy-mmode-define-minor-mode
 framov-mode
 "Toggle framov mode.
     With no argument, this command toggles the mode. 
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
 nil                ;; 初期値
 " framov"          ;; モード行への表示
 framov-mode-map    ;; キーマップ

 (when (and nil (assq 'framov-mode minor-mode-map-alist))
   (let ((framov-map nil)
	 (map-alist  minor-mode-map-alist))
     (while map-alist
       (when (eq (caar map-alist) 'framov-mode)
	 (setq framov-map (car map-alist))
	 (setcar map-alist (cadr map-alist))
	 (setcdr map-alist (cddr map-alist)))
       (setq map-alist (cdr map-alist)))
     (setq minor-mode-map-alist (cons framov-map map-alist)))))

(easy-mmode-define-global-mode
 global-framov-mode framov-mode
 (lambda () (framov-mode t)))



;;;----------------------------------------------------------------------

(provide 'framov)
;;; framov.el ends here
