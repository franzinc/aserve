;; -*- mode: common-lisp; package: net.aserve.examples -*-
;;
;; puzzle.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;

;; Description:
;;   Allegro Serve puzzle example


;; Original Author: Charles A. Cox, Franz Inc.



(defpackage puzzle
  (:use :common-lisp :excl))

(in-package :puzzle)

(eval-when (compile load eval)
  (require :aserve))

(defpackage puzzle
  (:use :net.html.generator :net.aserve))

(defparameter .directions.
    (make-array
     8
     :initial-contents '((-1 . -1)	; nw
			 (-1 . 0)	; n
			 (-1 . +1)	; ne
			 (0 . -1)	; w
			 (0 . +1)	; e
			 (+1 . -1)	; sw
			 (+1 . 0)	; s
			 (+1 . +1)	; se
			 )))

;; Bitmap of all Unicode characters whose name includes "letter".
(defparameter .unicode-letters-bm.
    (let ((a (make-array #.(expt 2 16) :element-type 'bit
			 :initial-element 0)))
      (dolist
	  (c '(#x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047 #x0048 #x0049
	       #x004a #x004b #x004c #x004d #x004e #x004f #x0050 #x0051 #x0052
	       #x0053 #x0054 #x0055 #x0056 #x0057 #x0058 #x0059 #x005a #x0061
	       #x0062 #x0063 #x0064 #x0065 #x0066 #x0067 #x0068 #x0069 #x006a
	       #x006b #x006c #x006d #x006e #x006f #x0070 #x0071 #x0072 #x0073
	       #x0074 #x0075 #x0076 #x0077 #x0078 #x0079 #x007a #x00c0 #x00c1
	       #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7 #x00c8 #x00c9 #x00ca
	       #x00cb #x00cc #x00cd #x00ce #x00cf #x00d0 #x00d1 #x00d2 #x00d3
	       #x00d4 #x00d5 #x00d6 #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd
	       #x00de #x00df #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6
	       #x00e7 #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
	       #x00f0 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f8 #x00f9
	       #x00fa #x00fb #x00fc #x00fd #x00fe #x00ff #x0100 #x0101 #x0102
	       #x0103 #x0104 #x0105 #x0106 #x0107 #x0108 #x0109 #x010a #x010b
	       #x010c #x010d #x010e #x010f #x0110 #x0111 #x0112 #x0113 #x0114
	       #x0115 #x0116 #x0117 #x0118 #x0119 #x011a #x011b #x011c #x011d
	       #x011e #x011f #x0120 #x0121 #x0122 #x0123 #x0124 #x0125 #x0126
	       #x0127 #x0128 #x0129 #x012a #x012b #x012c #x012d #x012e #x012f
	       #x0130 #x0131 #x0134 #x0135 #x0136 #x0137 #x0138 #x0139 #x013a
	       #x013b #x013c #x013d #x013e #x013f #x0140 #x0141 #x0142 #x0143
	       #x0144 #x0145 #x0146 #x0147 #x0148 #x0149 #x014a #x014b #x014c
	       #x014d #x014e #x014f #x0150 #x0151 #x0154 #x0155 #x0156 #x0157
	       #x0158 #x0159 #x015a #x015b #x015c #x015d #x015e #x015f #x0160
	       #x0161 #x0162 #x0163 #x0164 #x0165 #x0166 #x0167 #x0168 #x0169
	       #x016a #x016b #x016c #x016d #x016e #x016f #x0170 #x0171 #x0172
	       #x0173 #x0174 #x0175 #x0176 #x0177 #x0178 #x0179 #x017a #x017b
	       #x017c #x017d #x017e #x017f #x0180 #x0181 #x0182 #x0183 #x0184
	       #x0185 #x0186 #x0187 #x0188 #x0189 #x018a #x018b #x018c #x018d
	       #x018e #x018f #x0190 #x0191 #x0192 #x0193 #x0194 #x0195 #x0196
	       #x0197 #x0198 #x0199 #x019a #x019b #x019c #x019d #x019e #x019f
	       #x01a0 #x01a1 #x01a2 #x01a3 #x01a4 #x01a5 #x01a6 #x01a7 #x01a8
	       #x01a9 #x01aa #x01ab #x01ac #x01ad #x01ae #x01af #x01b0 #x01b1
	       #x01b2 #x01b3 #x01b4 #x01b5 #x01b6 #x01b7 #x01b8 #x01b9 #x01ba
	       #x01bb #x01bc #x01bd #x01be #x01bf #x01c0 #x01c1 #x01c2 #x01c3
	       #x01c4 #x01c5 #x01c6 #x01c7 #x01c8 #x01c9 #x01ca #x01cb #x01cc
	       #x01cd #x01ce #x01cf #x01d0 #x01d1 #x01d2 #x01d3 #x01d4 #x01d5
	       #x01d6 #x01d7 #x01d8 #x01d9 #x01da #x01db #x01dc #x01dd #x01de
	       #x01df #x01e0 #x01e1 #x01e2 #x01e3 #x01e4 #x01e5 #x01e6 #x01e7
	       #x01e8 #x01e9 #x01ea #x01eb #x01ec #x01ed #x01ee #x01ef #x01f0
	       #x01f1 #x01f2 #x01f3 #x01f4 #x01f5 #x01fa #x01fb #x01fc #x01fd
	       #x01fe #x01ff #x0200 #x0201 #x0202 #x0203 #x0204 #x0205 #x0206
	       #x0207 #x0208 #x0209 #x020a #x020b #x020c #x020d #x020e #x020f
	       #x0210 #x0211 #x0212 #x0213 #x0214 #x0215 #x0216 #x0217 #x0250
	       #x0251 #x0252 #x0253 #x0254 #x0255 #x0256 #x0257 #x0258 #x0259
	       #x025a #x025b #x025c #x025d #x025e #x025f #x0260 #x0261 #x0262
	       #x0263 #x0264 #x0265 #x0266 #x0267 #x0268 #x0269 #x026a #x026b
	       #x026c #x026d #x026e #x026f #x0270 #x0271 #x0272 #x0273 #x0274
	       #x0275 #x0276 #x0277 #x0278 #x0279 #x027a #x027b #x027c #x027d
	       #x027e #x027f #x0280 #x0281 #x0282 #x0283 #x0284 #x0285 #x0286
	       #x0287 #x0288 #x0289 #x028a #x028b #x028c #x028d #x028e #x028f
	       #x0290 #x0291 #x0292 #x0293 #x0294 #x0295 #x0296 #x0297 #x0298
	       #x0299 #x029a #x029b #x029c #x029d #x029e #x029f #x02a0 #x02a1
	       #x02a2 #x02a3 #x02a4 #x02a5 #x02a6 #x02a7 #x02a8 #x02b0 #x02b1
	       #x02b2 #x02b3 #x02b4 #x02b5 #x02b6 #x02b7 #x02b8 #x02b9 #x02ba
	       #x02bb #x02bc #x02bd #x02be #x02bf #x02c0 #x02c1 #x02c2 #x02c3
	       #x02c4 #x02c5 #x02c6 #x02c8 #x02c9 #x02ca #x02cb #x02cc #x02cd
	       #x02ce #x02cf #x02d0 #x02d1 #x02d2 #x02d3 #x02d4 #x02d5 #x02d6
	       #x02d7 #x02de #x02e0 #x02e1 #x02e2 #x02e3 #x02e4 #x02e5 #x02e6
	       #x02e7 #x02e8 #x02e9 #x0386 #x0388 #x0389 #x038a #x038c #x038e
	       #x038f #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
	       #x0398 #x0399 #x039a #x039b #x039c #x039d #x039e #x039f #x03a0
	       #x03a1 #x03a3 #x03a4 #x03a5 #x03a6 #x03a7 #x03a8 #x03a9 #x03aa
	       #x03ab #x03ac #x03ad #x03ae #x03af #x03b0 #x03b1 #x03b2 #x03b3
	       #x03b4 #x03b5 #x03b6 #x03b7 #x03b8 #x03b9 #x03ba #x03bb #x03bc
	       #x03bd #x03be #x03bf #x03c0 #x03c1 #x03c2 #x03c3 #x03c4 #x03c5
	       #x03c6 #x03c7 #x03c8 #x03c9 #x03ca #x03cb #x03cc #x03cd #x03ce
	       #x03da #x03dc #x03de #x03e0 #x03e2 #x03e3 #x03e4 #x03e5 #x03e6
	       #x03e7 #x03e8 #x03e9 #x03ea #x03eb #x03ec #x03ed #x03ee #x03ef
	       #x03f3 #x0401 #x0402 #x0403 #x0404 #x0405 #x0406 #x0407 #x0408
	       #x0409 #x040a #x040b #x040c #x040e #x040f #x0410 #x0411 #x0412
	       #x0413 #x0414 #x0415 #x0416 #x0417 #x0418 #x0419 #x041a #x041b
	       #x041c #x041d #x041e #x041f #x0420 #x0421 #x0422 #x0423 #x0424
	       #x0425 #x0426 #x0427 #x0428 #x0429 #x042a #x042b #x042c #x042d
	       #x042e #x042f #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436
	       #x0437 #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e #x043f
	       #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447 #x0448
	       #x0449 #x044a #x044b #x044c #x044d #x044e #x044f #x0451 #x0452
	       #x0453 #x0454 #x0455 #x0456 #x0457 #x0458 #x0459 #x045a #x045b
	       #x045c #x045e #x045f #x0460 #x0461 #x0462 #x0463 #x0464 #x0465
	       #x0466 #x0467 #x0468 #x0469 #x046a #x046b #x046c #x046d #x046e
	       #x046f #x0470 #x0471 #x0472 #x0473 #x0474 #x0475 #x0476 #x0477
	       #x0478 #x0479 #x047a #x047b #x047c #x047d #x047e #x047f #x0480
	       #x0481 #x0490 #x0491 #x0492 #x0493 #x0494 #x0495 #x0496 #x0497
	       #x0498 #x0499 #x049a #x049b #x049c #x049d #x049e #x049f #x04a0
	       #x04a1 #x04a2 #x04a3 #x04a6 #x04a7 #x04a8 #x04a9 #x04aa #x04ab
	       #x04ac #x04ad #x04ae #x04af #x04b0 #x04b1 #x04b2 #x04b3 #x04b6
	       #x04b7 #x04b8 #x04b9 #x04ba #x04bb #x04bc #x04bd #x04be #x04bf
	       #x04c0 #x04c1 #x04c2 #x04c3 #x04c4 #x04c7 #x04c8 #x04cb #x04cc
	       #x04d0 #x04d1 #x04d2 #x04d3 #x04d6 #x04d7 #x04d8 #x04d9 #x04da
	       #x04db #x04dc #x04dd #x04de #x04df #x04e0 #x04e1 #x04e2 #x04e3
	       #x04e4 #x04e5 #x04e6 #x04e7 #x04e8 #x04e9 #x04ea #x04eb #x04ee
	       #x04ef #x04f0 #x04f1 #x04f2 #x04f3 #x04f4 #x04f5 #x04f8 #x04f9
	       #x0531 #x0532 #x0533 #x0534 #x0535 #x0536 #x0537 #x0538 #x0539
	       #x053a #x053b #x053c #x053d #x053e #x053f #x0540 #x0541 #x0542
	       #x0543 #x0544 #x0545 #x0546 #x0547 #x0548 #x0549 #x054a #x054b
	       #x054c #x054d #x054e #x054f #x0550 #x0551 #x0552 #x0553 #x0554
	       #x0555 #x0556 #x0559 #x0561 #x0562 #x0563 #x0564 #x0565 #x0566
	       #x0567 #x0568 #x0569 #x056a #x056b #x056c #x056d #x056e #x056f
	       #x0570 #x0571 #x0572 #x0573 #x0574 #x0575 #x0576 #x0577 #x0578
	       #x0579 #x057a #x057b #x057c #x057d #x057e #x057f #x0580 #x0581
	       #x0582 #x0583 #x0584 #x0585 #x0586 #x05d0 #x05d1 #x05d2 #x05d3
	       #x05d4 #x05d5 #x05d6 #x05d7 #x05d8 #x05d9 #x05da #x05db #x05dc
	       #x05dd #x05de #x05df #x05e0 #x05e1 #x05e2 #x05e3 #x05e4 #x05e5
	       #x05e6 #x05e7 #x05e8 #x05e9 #x05ea #x0621 #x0622 #x0623 #x0624
	       #x0625 #x0626 #x0627 #x0628 #x0629 #x062a #x062b #x062c #x062d
	       #x062e #x062f #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636
	       #x0637 #x0638 #x0639 #x063a #x0641 #x0642 #x0643 #x0644 #x0645
	       #x0646 #x0647 #x0648 #x0649 #x064a #x0670 #x0671 #x0672 #x0673
	       #x0674 #x0675 #x0676 #x0677 #x0678 #x0679 #x067a #x067b #x067c
	       #x067d #x067e #x067f #x0680 #x0681 #x0682 #x0683 #x0684 #x0685
	       #x0686 #x0687 #x0688 #x0689 #x068a #x068b #x068c #x068d #x068e
	       #x068f #x0690 #x0691 #x0692 #x0693 #x0694 #x0695 #x0696 #x0697
	       #x0698 #x0699 #x069a #x069b #x069c #x069d #x069e #x069f #x06a0
	       #x06a1 #x06a2 #x06a3 #x06a4 #x06a5 #x06a6 #x06a7 #x06a8 #x06a9
	       #x06aa #x06ab #x06ac #x06ad #x06ae #x06af #x06b0 #x06b1 #x06b2
	       #x06b3 #x06b4 #x06b5 #x06b6 #x06b7 #x06ba #x06bb #x06bc #x06bd
	       #x06be #x06c0 #x06c1 #x06c2 #x06c3 #x06c4 #x06c5 #x06c6 #x06c7
	       #x06c8 #x06c9 #x06ca #x06cb #x06cc #x06cd #x06ce #x06d0 #x06d1
	       #x06d2 #x06d3 #x06d5 #x0905 #x0906 #x0907 #x0908 #x0909 #x090a
	       #x090b #x090c #x090d #x090e #x090f #x0910 #x0911 #x0912 #x0913
	       #x0914 #x0915 #x0916 #x0917 #x0918 #x0919 #x091a #x091b #x091c
	       #x091d #x091e #x091f #x0920 #x0921 #x0922 #x0923 #x0924 #x0925
	       #x0926 #x0927 #x0928 #x0929 #x092a #x092b #x092c #x092d #x092e
	       #x092f #x0930 #x0931 #x0932 #x0933 #x0934 #x0935 #x0936 #x0937
	       #x0938 #x0939 #x0958 #x0959 #x095a #x095b #x095c #x095d #x095e
	       #x095f #x0960 #x0961 #x0985 #x0986 #x0987 #x0988 #x0989 #x098a
	       #x098b #x098c #x098f #x0990 #x0993 #x0994 #x0995 #x0996 #x0997
	       #x0998 #x0999 #x099a #x099b #x099c #x099d #x099e #x099f #x09a0
	       #x09a1 #x09a2 #x09a3 #x09a4 #x09a5 #x09a6 #x09a7 #x09a8 #x09aa
	       #x09ab #x09ac #x09ad #x09ae #x09af #x09b0 #x09b2 #x09b6 #x09b7
	       #x09b8 #x09b9 #x09dc #x09dd #x09df #x09e0 #x09e1 #x09f0 #x09f1
	       #x0a05 #x0a06 #x0a07 #x0a08 #x0a09 #x0a0a #x0a0f #x0a10 #x0a13
	       #x0a14 #x0a15 #x0a16 #x0a17 #x0a18 #x0a19 #x0a1a #x0a1b #x0a1c
	       #x0a1d #x0a1e #x0a1f #x0a20 #x0a21 #x0a22 #x0a23 #x0a24 #x0a25
	       #x0a26 #x0a27 #x0a28 #x0a2a #x0a2b #x0a2c #x0a2d #x0a2e #x0a2f
	       #x0a30 #x0a32 #x0a33 #x0a35 #x0a36 #x0a38 #x0a39 #x0a59 #x0a5a
	       #x0a5b #x0a5c #x0a5e #x0a85 #x0a86 #x0a87 #x0a88 #x0a89 #x0a8a
	       #x0a8b #x0a8f #x0a90 #x0a93 #x0a94 #x0a95 #x0a96 #x0a97 #x0a98
	       #x0a99 #x0a9a #x0a9b #x0a9c #x0a9d #x0a9e #x0a9f #x0aa0 #x0aa1
	       #x0aa2 #x0aa3 #x0aa4 #x0aa5 #x0aa6 #x0aa7 #x0aa8 #x0aaa #x0aab
	       #x0aac #x0aad #x0aae #x0aaf #x0ab0 #x0ab2 #x0ab3 #x0ab5 #x0ab6
	       #x0ab7 #x0ab8 #x0ab9 #x0ae0 #x0b05 #x0b06 #x0b07 #x0b08 #x0b09
	       #x0b0a #x0b0b #x0b0c #x0b0f #x0b10 #x0b13 #x0b14 #x0b15 #x0b16
	       #x0b17 #x0b18 #x0b19 #x0b1a #x0b1b #x0b1c #x0b1d #x0b1e #x0b1f
	       #x0b20 #x0b21 #x0b22 #x0b23 #x0b24 #x0b25 #x0b26 #x0b27 #x0b28
	       #x0b2a #x0b2b #x0b2c #x0b2d #x0b2e #x0b2f #x0b30 #x0b32 #x0b33
	       #x0b36 #x0b37 #x0b38 #x0b39 #x0b5c #x0b5d #x0b5f #x0b60 #x0b61
	       #x0b85 #x0b86 #x0b87 #x0b88 #x0b89 #x0b8a #x0b8e #x0b8f #x0b90
	       #x0b92 #x0b93 #x0b94 #x0b95 #x0b99 #x0b9a #x0b9c #x0b9e #x0b9f
	       #x0ba3 #x0ba4 #x0ba8 #x0ba9 #x0baa #x0bae #x0baf #x0bb0 #x0bb1
	       #x0bb2 #x0bb3 #x0bb4 #x0bb5 #x0bb7 #x0bb8 #x0bb9 #x0c05 #x0c06
	       #x0c07 #x0c08 #x0c09 #x0c0a #x0c0b #x0c0c #x0c0e #x0c0f #x0c10
	       #x0c12 #x0c13 #x0c14 #x0c15 #x0c16 #x0c17 #x0c18 #x0c19 #x0c1a
	       #x0c1b #x0c1c #x0c1d #x0c1e #x0c1f #x0c20 #x0c21 #x0c22 #x0c23
	       #x0c24 #x0c25 #x0c26 #x0c27 #x0c28 #x0c2a #x0c2b #x0c2c #x0c2d
	       #x0c2e #x0c2f #x0c30 #x0c31 #x0c32 #x0c33 #x0c35 #x0c36 #x0c37
	       #x0c38 #x0c39 #x0c60 #x0c61 #x0c85 #x0c86 #x0c87 #x0c88 #x0c89
	       #x0c8a #x0c8b #x0c8c #x0c8e #x0c8f #x0c90 #x0c92 #x0c93 #x0c94
	       #x0c95 #x0c96 #x0c97 #x0c98 #x0c99 #x0c9a #x0c9b #x0c9c #x0c9d
	       #x0c9e #x0c9f #x0ca0 #x0ca1 #x0ca2 #x0ca3 #x0ca4 #x0ca5 #x0ca6
	       #x0ca7 #x0ca8 #x0caa #x0cab #x0cac #x0cad #x0cae #x0caf #x0cb0
	       #x0cb1 #x0cb2 #x0cb3 #x0cb5 #x0cb6 #x0cb7 #x0cb8 #x0cb9 #x0cde
	       #x0ce0 #x0ce1 #x0d05 #x0d06 #x0d07 #x0d08 #x0d09 #x0d0a #x0d0b
	       #x0d0c #x0d0e #x0d0f #x0d10 #x0d12 #x0d13 #x0d14 #x0d15 #x0d16
	       #x0d17 #x0d18 #x0d19 #x0d1a #x0d1b #x0d1c #x0d1d #x0d1e #x0d1f
	       #x0d20 #x0d21 #x0d22 #x0d23 #x0d24 #x0d25 #x0d26 #x0d27 #x0d28
	       #x0d2a #x0d2b #x0d2c #x0d2d #x0d2e #x0d2f #x0d30 #x0d31 #x0d32
	       #x0d33 #x0d34 #x0d35 #x0d36 #x0d37 #x0d38 #x0d39 #x0d60 #x0d61
	       #x0e81 #x0e82 #x0e84 #x0e87 #x0e88 #x0e8a #x0e8d #x0e94 #x0e95
	       #x0e96 #x0e97 #x0e99 #x0e9a #x0e9b #x0e9c #x0e9d #x0e9e #x0e9f
	       #x0ea1 #x0ea2 #x0ea3 #x0ea5 #x0ea7 #x0eaa #x0eab #x0ead #x0eae
	       #x0f40 #x0f41 #x0f42 #x0f43 #x0f44 #x0f45 #x0f46 #x0f47 #x0f49
	       #x0f4a #x0f4b #x0f4c #x0f4d #x0f4e #x0f4f #x0f50 #x0f51 #x0f52
	       #x0f53 #x0f54 #x0f55 #x0f56 #x0f57 #x0f58 #x0f59 #x0f5a #x0f5b
	       #x0f5c #x0f5d #x0f5e #x0f5f #x0f60 #x0f61 #x0f62 #x0f63 #x0f64
	       #x0f65 #x0f66 #x0f67 #x0f68 #x0f69 #x0f90 #x0f91 #x0f92 #x0f93
	       #x0f94 #x0f95 #x0f97 #x0f99 #x0f9a #x0f9b #x0f9c #x0f9d #x0f9e
	       #x0f9f #x0fa0 #x0fa1 #x0fa2 #x0fa3 #x0fa4 #x0fa5 #x0fa6 #x0fa7
	       #x0fa8 #x0fa9 #x0faa #x0fab #x0fac #x0fad #x0fb1 #x0fb2 #x0fb3
	       #x0fb4 #x0fb5 #x0fb6 #x0fb7 #x0fb9 #x10a0 #x10a1 #x10a2 #x10a3
	       #x10a4 #x10a5 #x10a6 #x10a7 #x10a8 #x10a9 #x10aa #x10ab #x10ac
	       #x10ad #x10ae #x10af #x10b0 #x10b1 #x10b2 #x10b3 #x10b4 #x10b5
	       #x10b6 #x10b7 #x10b8 #x10b9 #x10ba #x10bb #x10bc #x10bd #x10be
	       #x10bf #x10c0 #x10c1 #x10c2 #x10c3 #x10c4 #x10c5 #x10d0 #x10d1
	       #x10d2 #x10d3 #x10d4 #x10d5 #x10d6 #x10d7 #x10d8 #x10d9 #x10da
	       #x10db #x10dc #x10dd #x10de #x10df #x10e0 #x10e1 #x10e2 #x10e3
	       #x10e4 #x10e5 #x10e6 #x10e7 #x10e8 #x10e9 #x10ea #x10eb #x10ec
	       #x10ed #x10ee #x10ef #x10f0 #x10f1 #x10f2 #x10f3 #x10f4 #x10f5
	       #x10f6 #x1e00 #x1e01 #x1e02 #x1e03 #x1e04 #x1e05 #x1e06 #x1e07
	       #x1e08 #x1e09 #x1e0a #x1e0b #x1e0c #x1e0d #x1e0e #x1e0f #x1e10
	       #x1e11 #x1e12 #x1e13 #x1e14 #x1e15 #x1e16 #x1e17 #x1e18 #x1e19
	       #x1e1a #x1e1b #x1e1c #x1e1d #x1e1e #x1e1f #x1e20 #x1e21 #x1e22
	       #x1e23 #x1e24 #x1e25 #x1e26 #x1e27 #x1e28 #x1e29 #x1e2a #x1e2b
	       #x1e2c #x1e2d #x1e2e #x1e2f #x1e30 #x1e31 #x1e32 #x1e33 #x1e34
	       #x1e35 #x1e36 #x1e37 #x1e38 #x1e39 #x1e3a #x1e3b #x1e3c #x1e3d
	       #x1e3e #x1e3f #x1e40 #x1e41 #x1e42 #x1e43 #x1e44 #x1e45 #x1e46
	       #x1e47 #x1e48 #x1e49 #x1e4a #x1e4b #x1e4c #x1e4d #x1e4e #x1e4f
	       #x1e50 #x1e51 #x1e52 #x1e53 #x1e54 #x1e55 #x1e56 #x1e57 #x1e58
	       #x1e59 #x1e5a #x1e5b #x1e5c #x1e5d #x1e5e #x1e5f #x1e60 #x1e61
	       #x1e62 #x1e63 #x1e64 #x1e65 #x1e66 #x1e67 #x1e68 #x1e69 #x1e6a
	       #x1e6b #x1e6c #x1e6d #x1e6e #x1e6f #x1e70 #x1e71 #x1e72 #x1e73
	       #x1e74 #x1e75 #x1e76 #x1e77 #x1e78 #x1e79 #x1e7a #x1e7b #x1e7c
	       #x1e7d #x1e7e #x1e7f #x1e80 #x1e81 #x1e82 #x1e83 #x1e84 #x1e85
	       #x1e86 #x1e87 #x1e88 #x1e89 #x1e8a #x1e8b #x1e8c #x1e8d #x1e8e
	       #x1e8f #x1e90 #x1e91 #x1e92 #x1e93 #x1e94 #x1e95 #x1e96 #x1e97
	       #x1e98 #x1e99 #x1e9a #x1e9b #x1ea0 #x1ea1 #x1ea2 #x1ea3 #x1ea4
	       #x1ea5 #x1ea6 #x1ea7 #x1ea8 #x1ea9 #x1eaa #x1eab #x1eac #x1ead
	       #x1eae #x1eaf #x1eb0 #x1eb1 #x1eb2 #x1eb3 #x1eb4 #x1eb5 #x1eb6
	       #x1eb7 #x1eb8 #x1eb9 #x1eba #x1ebb #x1ebc #x1ebd #x1ebe #x1ebf
	       #x1ec0 #x1ec1 #x1ec2 #x1ec3 #x1ec4 #x1ec5 #x1ec6 #x1ec7 #x1ec8
	       #x1ec9 #x1eca #x1ecb #x1ecc #x1ecd #x1ece #x1ecf #x1ed0 #x1ed1
	       #x1ed2 #x1ed3 #x1ed4 #x1ed5 #x1ed6 #x1ed7 #x1ed8 #x1ed9 #x1eda
	       #x1edb #x1edc #x1edd #x1ede #x1edf #x1ee0 #x1ee1 #x1ee2 #x1ee3
	       #x1ee4 #x1ee5 #x1ee6 #x1ee7 #x1ee8 #x1ee9 #x1eea #x1eeb #x1eec
	       #x1eed #x1eee #x1eef #x1ef0 #x1ef1 #x1ef2 #x1ef3 #x1ef4 #x1ef5
	       #x1ef6 #x1ef7 #x1ef8 #x1ef9 #x1f00 #x1f01 #x1f02 #x1f03 #x1f04
	       #x1f05 #x1f06 #x1f07 #x1f08 #x1f09 #x1f0a #x1f0b #x1f0c #x1f0d
	       #x1f0e #x1f0f #x1f10 #x1f11 #x1f12 #x1f13 #x1f14 #x1f15 #x1f18
	       #x1f19 #x1f1a #x1f1b #x1f1c #x1f1d #x1f20 #x1f21 #x1f22 #x1f23
	       #x1f24 #x1f25 #x1f26 #x1f27 #x1f28 #x1f29 #x1f2a #x1f2b #x1f2c
	       #x1f2d #x1f2e #x1f2f #x1f30 #x1f31 #x1f32 #x1f33 #x1f34 #x1f35
	       #x1f36 #x1f37 #x1f38 #x1f39 #x1f3a #x1f3b #x1f3c #x1f3d #x1f3e
	       #x1f3f #x1f40 #x1f41 #x1f42 #x1f43 #x1f44 #x1f45 #x1f48 #x1f49
	       #x1f4a #x1f4b #x1f4c #x1f4d #x1f50 #x1f51 #x1f52 #x1f53 #x1f54
	       #x1f55 #x1f56 #x1f57 #x1f59 #x1f5b #x1f5d #x1f5f #x1f60 #x1f61
	       #x1f62 #x1f63 #x1f64 #x1f65 #x1f66 #x1f67 #x1f68 #x1f69 #x1f6a
	       #x1f6b #x1f6c #x1f6d #x1f6e #x1f6f #x1f70 #x1f71 #x1f72 #x1f73
	       #x1f74 #x1f75 #x1f76 #x1f77 #x1f78 #x1f79 #x1f7a #x1f7b #x1f7c
	       #x1f7d #x1f80 #x1f81 #x1f82 #x1f83 #x1f84 #x1f85 #x1f86 #x1f87
	       #x1f88 #x1f89 #x1f8a #x1f8b #x1f8c #x1f8d #x1f8e #x1f8f #x1f90
	       #x1f91 #x1f92 #x1f93 #x1f94 #x1f95 #x1f96 #x1f97 #x1f98 #x1f99
	       #x1f9a #x1f9b #x1f9c #x1f9d #x1f9e #x1f9f #x1fa0 #x1fa1 #x1fa2
	       #x1fa3 #x1fa4 #x1fa5 #x1fa6 #x1fa7 #x1fa8 #x1fa9 #x1faa #x1fab
	       #x1fac #x1fad #x1fae #x1faf #x1fb0 #x1fb1 #x1fb2 #x1fb3 #x1fb4
	       #x1fb6 #x1fb7 #x1fb8 #x1fb9 #x1fba #x1fbb #x1fbc #x1fc2 #x1fc3
	       #x1fc4 #x1fc6 #x1fc7 #x1fc8 #x1fc9 #x1fca #x1fcb #x1fcc #x1fd0
	       #x1fd1 #x1fd2 #x1fd3 #x1fd6 #x1fd7 #x1fd8 #x1fd9 #x1fda #x1fdb
	       #x1fe0 #x1fe1 #x1fe2 #x1fe3 #x1fe4 #x1fe5 #x1fe6 #x1fe7 #x1fe8
	       #x1fe9 #x1fea #x1feb #x1fec #x1ff2 #x1ff3 #x1ff4 #x1ff6 #x1ff7
	       #x1ff8 #x1ff9 #x1ffa #x1ffb #x1ffc #x207f #x210c #x2111 #x211c
	       #x2128 #x2129 #x212d #x249c #x249d #x249e #x249f #x24a0 #x24a1
	       #x24a2 #x24a3 #x24a4 #x24a5 #x24a6 #x24a7 #x24a8 #x24a9 #x24aa
	       #x24ab #x24ac #x24ad #x24ae #x24af #x24b0 #x24b1 #x24b2 #x24b3
	       #x24b4 #x24b5 #x24b6 #x24b7 #x24b8 #x24b9 #x24ba #x24bb #x24bc
	       #x24bd #x24be #x24bf #x24c0 #x24c1 #x24c2 #x24c3 #x24c4 #x24c5
	       #x24c6 #x24c7 #x24c8 #x24c9 #x24ca #x24cb #x24cc #x24cd #x24ce
	       #x24cf #x24d0 #x24d1 #x24d2 #x24d3 #x24d4 #x24d5 #x24d6 #x24d7
	       #x24d8 #x24d9 #x24da #x24db #x24dc #x24dd #x24de #x24df #x24e0
	       #x24e1 #x24e2 #x24e3 #x24e4 #x24e5 #x24e6 #x24e7 #x24e8 #x24e9
	       #x3041 #x3042 #x3043 #x3044 #x3045 #x3046 #x3047 #x3048 #x3049
	       #x304a #x304b #x304c #x304d #x304e #x304f #x3050 #x3051 #x3052
	       #x3053 #x3054 #x3055 #x3056 #x3057 #x3058 #x3059 #x305a #x305b
	       #x305c #x305d #x305e #x305f #x3060 #x3061 #x3062 #x3063 #x3064
	       #x3065 #x3066 #x3067 #x3068 #x3069 #x306a #x306b #x306c #x306d
	       #x306e #x306f #x3070 #x3071 #x3072 #x3073 #x3074 #x3075 #x3076
	       #x3077 #x3078 #x3079 #x307a #x307b #x307c #x307d #x307e #x307f
	       #x3080 #x3081 #x3082 #x3083 #x3084 #x3085 #x3086 #x3087 #x3088
	       #x3089 #x308a #x308b #x308c #x308d #x308e #x308f #x3090 #x3091
	       #x3092 #x3093 #x3094 #x30a1 #x30a2 #x30a3 #x30a4 #x30a5 #x30a6
	       #x30a7 #x30a8 #x30a9 #x30aa #x30ab #x30ac #x30ad #x30ae #x30af
	       #x30b0 #x30b1 #x30b2 #x30b3 #x30b4 #x30b5 #x30b6 #x30b7 #x30b8
	       #x30b9 #x30ba #x30bb #x30bc #x30bd #x30be #x30bf #x30c0 #x30c1
	       #x30c2 #x30c3 #x30c4 #x30c5 #x30c6 #x30c7 #x30c8 #x30c9 #x30ca
	       #x30cb #x30cc #x30cd #x30ce #x30cf #x30d0 #x30d1 #x30d2 #x30d3
	       #x30d4 #x30d5 #x30d6 #x30d7 #x30d8 #x30d9 #x30da #x30db #x30dc
	       #x30dd #x30de #x30df #x30e0 #x30e1 #x30e2 #x30e3 #x30e4 #x30e5
	       #x30e6 #x30e7 #x30e8 #x30e9 #x30ea #x30eb #x30ec #x30ed #x30ee
	       #x30ef #x30f0 #x30f1 #x30f2 #x30f3 #x30f4 #x30f5 #x30f6 #x30f7
	       #x30f8 #x30f9 #x30fa #x3105 #x3106 #x3107 #x3108 #x3109 #x310a
	       #x310b #x310c #x310d #x310e #x310f #x3110 #x3111 #x3112 #x3113
	       #x3114 #x3115 #x3116 #x3117 #x3118 #x3119 #x311a #x311b #x311c
	       #x311d #x311e #x311f #x3120 #x3121 #x3122 #x3123 #x3124 #x3125
	       #x3126 #x3127 #x3128 #x3129 #x312a #x312b #x312c #x3131 #x3132
	       #x3133 #x3134 #x3135 #x3136 #x3137 #x3138 #x3139 #x313a #x313b
	       #x313c #x313d #x313e #x313f #x3140 #x3141 #x3142 #x3143 #x3144
	       #x3145 #x3146 #x3147 #x3148 #x3149 #x314a #x314b #x314c #x314d
	       #x314e #x314f #x3150 #x3151 #x3152 #x3153 #x3154 #x3155 #x3156
	       #x3157 #x3158 #x3159 #x315a #x315b #x315c #x315d #x315e #x315f
	       #x3160 #x3161 #x3162 #x3163 #x3165 #x3166 #x3167 #x3168 #x3169
	       #x316a #x316b #x316c #x316d #x316e #x316f #x3170 #x3171 #x3172
	       #x3173 #x3174 #x3175 #x3176 #x3177 #x3178 #x3179 #x317a #x317b
	       #x317c #x317d #x317e #x317f #x3180 #x3181 #x3182 #x3183 #x3184
	       #x3185 #x3186 #x3187 #x3188 #x3189 #x318a #x318b #x318c #x318d
	       #x318e #xfb20 #xfb21 #xfb22 #xfb23 #xfb24 #xfb25 #xfb26 #xfb27
	       #xfb28 #xfb29 #xfb2a #xfb2b #xfb2c #xfb2d #xfb2e #xfb2f #xfb30
	       #xfb31 #xfb32 #xfb33 #xfb34 #xfb35 #xfb36 #xfb38 #xfb39 #xfb3a
	       #xfb3b #xfb3c #xfb3e #xfb40 #xfb41 #xfb43 #xfb44 #xfb46 #xfb47
	       #xfb48 #xfb49 #xfb4a #xfb4b #xfb4c #xfb4d #xfb4e #xfb50 #xfb51
	       #xfb52 #xfb53 #xfb54 #xfb55 #xfb56 #xfb57 #xfb58 #xfb59 #xfb5a
	       #xfb5b #xfb5c #xfb5d #xfb5e #xfb5f #xfb60 #xfb61 #xfb62 #xfb63
	       #xfb64 #xfb65 #xfb66 #xfb67 #xfb68 #xfb69 #xfb6a #xfb6b #xfb6c
	       #xfb6d #xfb6e #xfb6f #xfb70 #xfb71 #xfb72 #xfb73 #xfb74 #xfb75
	       #xfb76 #xfb77 #xfb78 #xfb79 #xfb7a #xfb7b #xfb7c #xfb7d #xfb7e
	       #xfb7f #xfb80 #xfb81 #xfb82 #xfb83 #xfb84 #xfb85 #xfb86 #xfb87
	       #xfb88 #xfb89 #xfb8a #xfb8b #xfb8c #xfb8d #xfb8e #xfb8f #xfb90
	       #xfb91 #xfb92 #xfb93 #xfb94 #xfb95 #xfb96 #xfb97 #xfb98 #xfb99
	       #xfb9a #xfb9b #xfb9c #xfb9d #xfb9e #xfb9f #xfba0 #xfba1 #xfba2
	       #xfba3 #xfba4 #xfba5 #xfba6 #xfba7 #xfba8 #xfba9 #xfbaa #xfbab
	       #xfbac #xfbad #xfbae #xfbaf #xfbb0 #xfbb1 #xfbd3 #xfbd4 #xfbd5
	       #xfbd6 #xfbd7 #xfbd8 #xfbd9 #xfbda #xfbdb #xfbdc #xfbdd #xfbde
	       #xfbdf #xfbe0 #xfbe1 #xfbe2 #xfbe3 #xfbe4 #xfbe5 #xfbe6 #xfbe7
	       #xfbe8 #xfbe9 #xfbfc #xfbfd #xfbfe #xfbff #xfe80 #xfe81 #xfe82
	       #xfe83 #xfe84 #xfe85 #xfe86 #xfe87 #xfe88 #xfe89 #xfe8a #xfe8b
	       #xfe8c #xfe8d #xfe8e #xfe8f #xfe90 #xfe91 #xfe92 #xfe93 #xfe94
	       #xfe95 #xfe96 #xfe97 #xfe98 #xfe99 #xfe9a #xfe9b #xfe9c #xfe9d
	       #xfe9e #xfe9f #xfea0 #xfea1 #xfea2 #xfea3 #xfea4 #xfea5 #xfea6
	       #xfea7 #xfea8 #xfea9 #xfeaa #xfeab #xfeac #xfead #xfeae #xfeaf
	       #xfeb0 #xfeb1 #xfeb2 #xfeb3 #xfeb4 #xfeb5 #xfeb6 #xfeb7 #xfeb8
	       #xfeb9 #xfeba #xfebb #xfebc #xfebd #xfebe #xfebf #xfec0 #xfec1
	       #xfec2 #xfec3 #xfec4 #xfec5 #xfec6 #xfec7 #xfec8 #xfec9 #xfeca
	       #xfecb #xfecc #xfecd #xfece #xfecf #xfed0 #xfed1 #xfed2 #xfed3
	       #xfed4 #xfed5 #xfed6 #xfed7 #xfed8 #xfed9 #xfeda #xfedb #xfedc
	       #xfedd #xfede #xfedf #xfee0 #xfee1 #xfee2 #xfee3 #xfee4 #xfee5
	       #xfee6 #xfee7 #xfee8 #xfee9 #xfeea #xfeeb #xfeec #xfeed #xfeee
	       #xfeef #xfef0 #xfef1 #xfef2 #xfef3 #xfef4 #xff21 #xff22 #xff23
	       #xff24 #xff25 #xff26 #xff27 #xff28 #xff29 #xff2a #xff2b #xff2c
	       #xff2d #xff2e #xff2f #xff30 #xff31 #xff32 #xff33 #xff34 #xff35
	       #xff36 #xff37 #xff38 #xff39 #xff3a #xff41 #xff42 #xff43 #xff44
	       #xff45 #xff46 #xff47 #xff48 #xff49 #xff4a #xff4b #xff4c #xff4d
	       #xff4e #xff4f #xff50 #xff51 #xff52 #xff53 #xff54 #xff55 #xff56
	       #xff57 #xff58 #xff59 #xff5a #xff66 #xff67 #xff68 #xff69 #xff6a
	       #xff6b #xff6c #xff6d #xff6e #xff6f #xff71 #xff72 #xff73 #xff74
	       #xff75 #xff76 #xff77 #xff78 #xff79 #xff7a #xff7b #xff7c #xff7d
	       #xff7e #xff7f #xff80 #xff81 #xff82 #xff83 #xff84 #xff85 #xff86
	       #xff87 #xff88 #xff89 #xff8a #xff8b #xff8c #xff8d #xff8e #xff8f
	       #xff90 #xff91 #xff92 #xff93 #xff94 #xff95 #xff96 #xff97 #xff98
	       #xff99 #xff9a #xff9b #xff9c #xff9d #xffa1 #xffa2 #xffa3 #xffa4
	       #xffa5 #xffa6 #xffa7 #xffa8 #xffa9 #xffaa #xffab #xffac #xffad
	       #xffae #xffaf #xffb0 #xffb1 #xffb2 #xffb3 #xffb4 #xffb5 #xffb6
	       #xffb7 #xffb8 #xffb9 #xffba #xffbb #xffbc #xffbd #xffbe #xffc2
	       #xffc3 #xffc4 #xffc5 #xffc6 #xffc7 #xffca #xffcb #xffcc #xffcd
	       #xffce #xffcf #xffd2 #xffd3 #xffd4 #xffd5 #xffd6 #xffd7 #xffda
	       #xffdb #xffdc))
	(setf (aref a c) 1))
      a))

(defmacro cjk-p (code)
  `(or
    ;; CJK Ideographs
    (<= #x4e00 ,code #x9fff)
    ;; Hangul Syllables
    (<= #xac00 ,code #xd7a3)))

(defmacro puzzle-rows (puzzle)
  `(first (array-dimensions ,puzzle)))

(defmacro puzzle-cols (puzzle)
  `(second (array-dimensions ,puzzle)))

(defun get-random-dir ()
  (aref .directions. (random 8)))

(defun get-random-start (puzzle)
  (cons (random (puzzle-rows puzzle))
	(random (puzzle-cols puzzle))))

;; Insert a word into a puzzle.
(defun insert (word puzzle &key (install nil)
				(dir (get-random-dir))
				(start (get-random-start puzzle))
				(attempt 0)
				(extend-limit 0)
				(attempt-limit 100)
	       &aux (length (length word))
		    (roff 0)
		    (coff 0))
  (macrolet ((retry ()
	       `(progn
		  (incf attempt)
		  (setq start (get-random-start puzzle)
			dir (get-random-dir))
		  (go :restart))))
    (tagbody
     :restart
      (do ((row (car start) (+ row (car dir)))
	   (col (cdr start) (+ col (cdr dir)))
	   (i 0 (1+ i)))
	  ((>= i (length word))
	   ;; if we're not already installing, then we arrive here when
	   ;; we've passed all the tests and can begin installing.
	   (if* (not install)
	      then (setq install t)
		   (go :restart)))
	;; If we're installing, then just slap in the letter.  Otherwise,
	;; check  if the letter fits and/or if the puzzle needs extending.
	(if* install
	   then (setf (aref puzzle row col) (schar word i))
	   else (if* (or (< row 0)
			 (< col 0)
			 (>= row (first (array-dimensions puzzle)))
			 (>= col (second (array-dimensions puzzle)))
			 (>= attempt attempt-limit))
		   then ;; Don't allow puzzle size to extend unless we've tried
			;; several attempts.
			(if* (>= attempt attempt-limit)
			   then (incf extend-limit)
				(setq attempt 0))
			(multiple-value-bind (npuzzle nroff ncoff)
			    ;; We add 1 randomly to the row extension and to
			    ;; the column extension to work around the problem where
			    ;; the puzzle may already be completely full.
			    (extend-puzzle puzzle
					   extend-limit
					   (+ (car start) (* (car dir)
							     (- length (random 2))))
					   (+ (cdr start) (* (cdr dir)
							     (- length (random 2)))))
			  (if* npuzzle
			     then (setq puzzle npuzzle)
				  (incf roff nroff)
				  (incf coff ncoff)
				  (incf row nroff) (incf (car start) nroff)
				  (incf col ncoff) (incf (cdr start) ncoff)
			     else ;; extend-puzzle rejected because of
				  ;; extend-limit, so we just loop around to
				  ;; try again...
				  (retry))))
		(if* (and (aref puzzle row col)
			  (not (eq (aref puzzle row col)
				   (schar word i))))
		   then ;; existing letters in puzzle didn't match.  So we
			;; try again...
			(retry)))))
    (values puzzle start dir roff coff)))

(defun extend-puzzle (puzzle extend-limit erow ecol
		      &aux (prows (puzzle-rows puzzle))
			   (pcols (puzzle-cols puzzle)))
  (let* ((shift-rows (if* (minusp erow)
			then (- erow)))
	 (shift-cols (if* (minusp ecol)
			then (- ecol)))
	 (new-rows (+ prows (or shift-rows (max 0 (- (1+ erow) prows)))))
	 (new-cols (+ pcols (or shift-cols (max 0 (- (1+ ecol) pcols))))))
    (if* (or (> new-rows extend-limit)
	     (> new-cols extend-limit))
       then ;; reject
	    (return-from extend-puzzle nil))
    (setq shift-rows (or shift-rows 0))
    (setq shift-cols (or shift-cols 0))
    (setq puzzle (adjust-array puzzle (list new-rows new-cols)
			       :initial-element nil))
    (if* (or (minusp erow) (minusp ecol))
       then (do ((r (- new-rows shift-rows 1) (1- r)))
		((< r 0))
	      (do ((c (- new-cols shift-cols 1) (1- c)))
		  ((< c 0))
		(setf (aref puzzle (+ r shift-rows) (+ c shift-cols))
		  (aref puzzle r c)))
	      (do ((c 0 (1+ c)))
		  ((>= c shift-cols))
		(setf (aref puzzle (+ r shift-rows) c) nil)))
	    (do ((r 0 (1+ r)))
		((>= r shift-rows))
	      (do ((c 0 (1+ c)))
		  ((>= c new-cols))
		(setf (aref puzzle r c) nil))))
    (values puzzle shift-rows shift-cols)))

(defun make-puzzle (word-list fill)
  ;; We actually make the puzzle twice, throwing away the first one after
  ;; getting its size.  The idea is that words are likely to be more evenly
  ;; distributed in the second puzzle.
  (if* (not word-list)
     then (return-from make-puzzle nil))
  (let ((puzzle (make-puzzle-1 word-list
			       (make-array '(1 1)
					   :initial-element nil
					   :adjustable t)
			       "none")))
    (make-puzzle-1 word-list
		   (make-array (array-dimensions puzzle)
			       :initial-element nil
			       :adjustable t)
		   fill)))

(defun make-puzzle-1 (word-list puzzle fill
		      &aux (answers nil)
			   (fill-sym (intern fill :keyword)))
  (dolist (word word-list)
    (multiple-value-bind (npuzzle start dir roff coff) (insert word puzzle)
      (setq puzzle npuzzle)
      (dolist (a answers)
	(incf (car (second a)) roff)
	(incf (cdr (second a)) coff))
      (push (list word start dir) answers)))
  (dotimes (i (apply #'* (array-dimensions puzzle)))
    (if* (not (row-major-aref puzzle i))
       then (setf (row-major-aref puzzle i)
	
	      (ecase fill-sym
		(:|ascii-lc| (code-char (+ (random 26) #.(char-code #\a))))
		(:|none| #\space)
		(:|unicode-nocjk| (loop (let ((c (random #.(expt 2 16))))
					  (if* (= 1 (aref .unicode-letters-bm. c))
					     then (return (code-char c))))))
		(:|unicode-cjk| (loop (let ((c (random #.(expt 2 16))))
					(if* (= 1 (aref .unicode-letters-bm. c))
					   then (return (code-char c)))
					(if* (cjk-p c)
					   then (return (code-char c))))))))))
  (values puzzle
	  (coerce
	   (sort answers #'(lambda (x y)
			     (string< (car x) (car y))))
	   'array)))
    
    
(defun mark-puzzle (puzzle index answers)
  (let* ((answer (aref answers index))
	 (start (second answer))
	 (dir (third answer))
	 (length (length (car answer)))
	 (row (car start))
	 (col (cdr start)))
    (dotimes (i length)
      (setf (aref puzzle row col) (cons (aref puzzle row col) nil))
      (incf row (car dir))
      (incf col (cdr dir)))))


(defun unmark (puzzle row col)
  (if* (consp (aref puzzle row col))
     then (setf (aref puzzle row col) (car (aref puzzle row col)))
	  t))

(defun words-list (words-string)
  (do ((words nil)
       (words-chars (coerce words-string 'list)))
      ((null words-chars) (nreverse words))
    (let ((word nil))
      (loop
	(let ((char (pop words-chars)))
	  (if* (or (null char)
		   (member char '(#\space #\newline #\tab #\return
				  #\linefeed)))
	     then (push (coerce (nreverse word) 'string) words)
		  (return)
	     else (push char word)))))))


(defun cannot-do-puzzle (req ent)
  (with-http-response (req ent)
       (with-http-body (req ent)
	 (princ #.(format nil "~
This page available only with International Allegro CL post 6.0 beta")
		*html-stream*))))


(defun can-do-puzzle (req ent)
  (let ((puzzle-url (symbol-name (gensym "/wordpuzzle")))
	(puzzle nil)
	(answers nil))
    ;; publish new url on the fly.
    ;; Enhancement To Do: Allow puzzles to be deallocated, either by timeout
    ;; or some other mechanism.
     
    (publish
     :path puzzle-url
     :content-type "text/html; charset=utf-8"
     :function
     #'(lambda (req ent &aux (marked nil))
	 (let ((lookup
		(assoc "index" (request-query req :external-format
					      :utf8-base)
		       :test #'string=)))
	   (if* lookup
	      then (setq marked t)
		   (mark-puzzle puzzle (read-from-string (cdr lookup)) 
				answers)))
	 (let* ((rq (request-query req :external-format :utf8-base))
		(words-string (cdr (assoc "words" rq :test #'string=)))
		(fill (cdr (assoc "fill" rq :test #'string=))))
	   (if* words-string
	      then (multiple-value-setq (puzzle answers)
		     (make-puzzle (words-list words-string) fill))))
	 (with-http-response (req ent)
	   (with-http-body (req ent
				:external-format :utf8-base)
	     (html
	      (:html
	       (:head (:title "Puzzle"))
	       (:body
		(:p #.(format nil "~
Characters that appear as dots or empty boxes or question-marks likely look
that way because your browser is missing the needed font(s)."))
		(if* puzzle
		   then (html
			 (:center
			  ((:table border 0 width "75%")
			   (:tr (:td #.(format nil "~
Click on letter in puzzle to see its character description."))
				(:td #.(format nil "~
Click on word to see its puzzle location.")))
			   (:tr
			    (:td
			     ((:table border 0)
			      (dotimes (r (puzzle-rows puzzle))
				(html
				 (:tr
				  (dotimes (c (puzzle-cols puzzle))
				    (html
				     ((:td :if* (unmark puzzle r c)
					   :bgcolor "lime")
				      ((:a href
					   (format nil "/wp_echo?char=~a"
						   (uriencode-string
						    (format
						     nil "u+~4,'0x:~s"
						     (char-code
						      (aref puzzle r c))
						     (aref puzzle r c)))))
				       (:tt (:princ
					     (aref puzzle r c))))))))))))
			    (:td
			     ((:table border 0)
			      (dotimes (i (length answers))
				(let ((url (format nil "~a?index=~a"
						   puzzle-url i)))
				  (html
				   (:tr
				    (:td
				     ((:a href url)
				      (:princ
				       (car
					(aref answers i)))))))))))))))
		   else (html
			 (:p "No words entered")))
		(:p ((:a :href "/wordpuzzle") "New Puzzle"))
		(if* marked
		   then (html
			 (:p ((:a :href puzzle-url) "Clear Answer")))))))))))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(html (:html
	       (:head (:title "Enter Words"))
	       (:body
		(:p
		 #.(format nil "~
Enter words separated by spaces or newlines.  Click on `make puzzle' button ~
below to generate the puzzle."))
		((:form :action puzzle-url
			:method "POST")
		 ((:textarea :name "words" :rows 15 :cols 50))
		 (:dl
		  (:dt "Please select category of fill letters:")
		  (:dd ((:input :type "radio"
				:name "fill"
				:value "ascii-lc"
				:checked "checked")
			"English Lower Case Only."))
		  (:dd ((:input :type "radio"
				:name "fill"
				:value "unicode-nocjk")
			"All Unicode Letters "
			(:em "except ")
			"Chinese-Japanese-Korean ideographs."))
		  (:dd ((:input :type "radio"
				:name "fill"
				:value "unicode-cjk")
			"All Unicode Letters "
			(:em "including ")
			"Chinese-Japanese-Korean ideographs."))
		  (:dd ((:input :type "radio"
				:name "fill"
				:value "none")
			"No fill characters.")))
		 ((:input :type "submit"
			  :value "make puzzle"))
		 (:p #.(format nil "~
Below are links containing international character samples you can use to copy
and paste into the word list.
Note that even characters that don't display (due to missing fonts) can still
be copied and pasted."))
		 (:ul (:li ((:a href #.(format nil "~
http://www.columbia.edu/kermit/utf8.html")
				target "_blank")
			    "UTF-8 Sampler"))
		      (:li ((:a href #.(format nil "~
http://www.trigeminal.com/samples/provincial.html")
				target "_blank")
			    #.(format nil "~
The \"anyone can be provincial!\" page"))))))))))))


;;
;; the entry link to this demo:
;;
(publish
 :path "/wordpuzzle"
 :content-type "text/html; charset=utf-8"
 :function
 #-(and allegro ics (version>= 6 0 pre-final 1))
 #'(lambda (req ent)
     (cannot-do-puzzle req ent))


 #+(and allegro ics (version>= 6 0 pre-final 1))
 #'(lambda (req ent)
     ; test at runtime in case we compiled with an international lisp
     ; and are running in an 8bit lisp
     (if* (member :ics *features* :test #'eq)
	then (can-do-puzzle req ent)
	else (cannot-do-puzzle req ent))))


(publish
 :path "/wp_echo"
 :content-type "text/html; charset=utf-8"
 :function
 #'(lambda (req ent)
     (let ((lookup
	    (assoc "char" (request-query req)
		   :test #'string=)))
       (if* lookup
	  then (setq lookup
		 (let ((*read-base* 16))
		   (read-from-string
		    (subseq (cdr lookup)
			    #.(length "u+")
			    #.(length "u+xxxx"))))))
       (with-http-response (req ent)
	 (with-http-body (req ent
			      :external-format :utf8-base)
	   (html
	    (:html
	     (:head (:title "Character Description"))
	     (:body
	      (:p
	       (:princ (format nil "Unicode value:  U+~4,'0x"
			       lookup)))
	      (:p
	       "Lisp Character Name:  "
	       ((:font :size "+3")
		(:prin1 (code-char lookup))))
	      (:p
	       "Browser Font Display:  "
	       ((:font :size "+3")
		(:princ (code-char lookup)))
	       :br
	       #.(format nil "~
Characters that appear as dots or empty boxes or question-marks likely look
that way because your browser is missing the needed font(s)."))
	      (let ((uglyph (format nil "~
http://charts.unicode.org/Glyphs/~2,'0x/U~4,'0x.gif"
				    (ldb (byte 8 8) lookup)
				    lookup)))
		(html ((:table border 0)
		       (:tr
			(:td #.(format nil "~
Glyph GIF (from Unicode web site -- not all characters have gifs):")
			     :br
			     (:princ (format nil "[Loading from `~a'.]"
					     uglyph)))
			(:td
			 ((:img :src uglyph
				:alt (format nil "~s" (code-char lookup))
				:border 2)))))))
	      (if* (cjk-p lookup)
		 then (if* (<= #xac00 lookup #xd7a3)
			 then (html (:p "Character is a Hangul Syllable."))
			 else (html (:p #.(format nil "~
Character is an ideograph from Chinese, Japanese, or Korean.")))))
	      (:p #.(format nil "~
Use browser `Back' button to return to puzzle."))))))))))
