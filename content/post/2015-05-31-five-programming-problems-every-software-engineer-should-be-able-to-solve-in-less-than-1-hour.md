+++
title = "Emacs Lispで「ソフトウェアエンジニアならば1時間（略）」を解いてみた 二番煎じ"
author = ["Tsunenobu Kai"]
date = 2015-05-31
tags = ["Emacs"]
draft = false
+++

[Emacs Lispで「ソフトウェアエンジニアならば1時間以内に解けなければいけない5つの問題」を解いてみた - Life is very short](http://d.hatena.ne.jp/syohex/touch/20150529/1432908287)という記事を見かけて、面白そうだったので自分でも解いてみました。二番煎じです。

上記事では `(require 'cl-lib)` してcl系マクロを使ってやってるみたいだったので、cl-libなし縛りでやってみました。

-   発端 [Five programming problems every Software Engineer should be able to solve in less than 1 hour](https://blog.svpino.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour)

<!--more-->


## 問題1 {#問題1}

```emacs-lisp
;; Write three functions that compute the sum of the numbers in a given list
;; using a for-loop, a while-loop, and recursion.
(defun for-sum (lst)
  (let ((sum 0))
    (dolist (elm lst sum)
      (setq sum (+ sum elm)))))
(for-sum '(-1 0 1 2))                   ; => 2

(defun while-sum (lst)
  (let ((sum 0))
    (while lst
      (setq sum (+ sum (car lst)))
      (setq lst (cdr lst)))
    sum))
(while-sum '(-1 0 1 2))                 ; => 2

(defun recursive-sum (lst)
  (if (null lst)
      0
    (+ (car lst) (recursive-sum (cdr lst)))))
(recursive-sum '(-1 0 1 2))             ; => 2
```


## 問題2 {#問題2}

```emacs-lisp
;; Write a function that combines two lists by alternatingly taking
;; elements. For example: given the two lists [a, b, c] and [1, 2, 3], the
;; function should return [a, 1, b, 2, c, 3].
(defun alternate-cat (lst1 lst2)
  (let ((min-length (min (length lst1) (length lst2)))
	combined)
    (dotimes (i min-length)
      (push (nth i lst1) combined)
      (push (nth i lst2) combined))
    ;; append a rest of longer list
    (append (reverse combined)
	    (nthcdr min-length
		    (if (< (length lst1) (length lst2)) lst2 lst1)))))
(alternate-cat '(a b c) '(1 2 3))       ; => (a 1 b 2 c 3)
(alternate-cat '(a b c d e f) '(1 2 3)) ; => (a 1 b 2 c 3 d e f)
(alternate-cat '(a b c) '(1 2 3 4 5))   ; => (a 1 b 2 c 3 4 5)
```

問題5で再利用する都合上、2つのリストの長さが異なる場合は、長いリストの残りの部分を末尾に並べるようにしました。

cl-loopマクロがないと、複数のリストを同時にループさせるのが辛い。


## 問題3 {#問題3}

```emacs-lisp
;; Write a function that computes the list of the first 100 Fibonacci
;; numbers. By definition, the first two numbers in the Fibonacci sequence
;; are 0 and 1, and each subsequent number is the sum of the previous two. As
;; an example, here are the first 10 Fibonnaci numbers: 0, 1, 1, 2, 3, 5, 8,
;; 13, 21, and 34.
(require 'calc)
(defun fibonacci-list (n)
  (let (fib-list)
    (dotimes (i n)
      (if (memql i '(0 1))
	  (push i fib-list)
	(push (math-add (car fib-list) (cadr fib-list)) fib-list)))
    ;; Convert to string for printing
    (mapcar #'math-format-number (reverse fib-list))))
(fibonacci-list 100)                     ; => ("0" "1" "1" "2" "3" "5" "8" "13" "21" "34" "55" "89" "144" "233" "377" "610" "987" "1597" "2584" "4181" "6765" "10946" "17711" "28657" "46368" "75025" "121393" "196418" "317811" "514229" "832040" "1346269" "2178309" "3524578" "5702887" "9227465" "14930352" "24157817" "39088169" "63245986" "102334155" "165580141" "267914296" "433494437" "701408733" "1134903170" "1836311903" "2971215073" "4807526976" "7778742049" "12586269025" "20365011074" "32951280099" "53316291173" "86267571272" "139583862445" "225851433717" "365435296162" "591286729879" "956722026041" "1548008755920" "2504730781961" "4052739537881" "6557470319842" "10610209857723" "17167680177565" "27777890035288" "44945570212853" "72723460248141" "117669030460994" "190392490709135" "308061521170129" "498454011879264" "806515533049393" "1304969544928657" "2111485077978050" "3416454622906707" "5527939700884757" "8944394323791464" "14472334024676221" "23416728348467685" "37889062373143906" "61305790721611591" "99194853094755497" "160500643816367088" "259695496911122585" "420196140727489673" "679891637638612258" "1100087778366101931" "1779979416004714189" "2880067194370816120" "4660046610375530309" "7540113804746346429" "12200160415121876738" "19740274219868223167" "31940434634990099905" "51680708854858323072" "83621143489848422977" "135301852344706746049" "218922995834555169026")
```

フィボナッチ数列の100番目は `218922995834555169026` なわけですが、Emacs Lispの整数上限を軽く超えています。なので上記事と同様 `calc.el` を使って任意桁整数を扱えるようにしています。

この問題はフィボナッチ数列の計算ではなく、任意桁整数の扱い方を確認する意図があるのかな、という気がしました。


## 問題4 {#問題4}

```emacs-lisp
;; Write a function that given a list of non negative integers, arranges them
;; such that they form the largest possible number. For example, given [50,
;; 2, 1, 9], the largest formed number is 95021.
(defun largest-combined-number (lst)
  (string-to-number
   (mapconcat #'identity
	      ;; sort descending dictionary order
	      (sort (mapcar #'number-to-string lst)
		    (lambda (elm1 elm2) (string< elm2 elm1)))
	      "")))
(largest-combined-number '(50 2 1 9))   ; => 95021
```

辞書順にソートするのを思いつけばすぐ解けました。


## 問題5 {#問題5}

```emacs-lisp
;; Write a program that outputs all possibilities to put + or - or nothing
;; between the numbers 1, 2, ..., 9 (in this order) such that the result is
;; always 100. For example: 1 + 2 + 34 – 5 + 67 – 8 + 9 = 100.
(defun output-all-expressions (expected)
  (let* ((numbers (mapcar #'number-to-string (number-sequence 1 9)))
	 (ops '("+" "-" ""))
	 ;; all permutations of binary operators
	 (ops-permutations (list-permutations ops (1- (length numbers)))))
    (dolist (ops-perm ops-permutations)
      (let (;; insert ops between numbers
	    (expression (mapconcat #'identity                         ;                 (1)
				   (alternate-cat numbers ops-perm)
				   ""))
	    (start 0)
	    lst)
	;; separate the expression into a list of numbers
	(while (string-match "[-+]?[0-9]+" expression start)
	  (push (string-to-number (match-string 0 expression)) lst)
	  (setq start (match-end 0)))
	(when (= (for-sum lst) expected)
	  (insert (format "%s=%s\n" expression expected)))
	))))

(defun list-permutations (lst n)
  (if (< n 1)
      nil
    (if (= n 1)
	(mapcar #'list lst)
      (let (result)
	(dolist (elm lst result)
	  (setq result (append result
			       (mapcar (lambda (x) (cons elm x))
				       (list-permutations lst (1- n)))))))
      )))
```

`(output-all-expressions 100)` を評価すると以下のように出力されます。たしかに `1 + 2 + 34 – 5 + 67 - 8 + 9 = 100` が含まれています。

```text
1+2+3-4+5+6+78+9=100
1+2+34-5+67-8+9=100
1+23-4+5+6+78-9=100
1+23-4+56+7+8+9=100
12+3+4+5-6-7+89=100
12+3-4+5+67+8+9=100
12-3-4+5-6+7+89=100
123+4-5+67-89=100
123+45-67+8-9=100
123-4-5-6-7+8-9=100
123-45-67+89=100
```

問題1と問題2の関数を再利用しています。

なんとなく演算子の順列を全部試せばいいんだろうな、とは思いついたものの順列組み合わせを算出する関数(`list-permutations`)を書くのにかなり時間がかかりました。関数プログラミング的なリスト操作関数を自分で実装するというのに慣れてなくて大変手間取りました。

また数値と演算子のリストからどうやって結果を算出するかも悩ましいところでした。リストを何とかしてポーランド式とか逆ポーランド式の木構造に加工すればいいのか、など考えたもののcl-libが使えないのではそれもままならなさそうだと感じられた。なのでまず数式を文字列で作ってしまって、それを前からパースする単純な方法にしました。


## 感想 {#感想}

問題1〜4で1時間かかって、5でさらに1時間ぐらいかかりました。残念ながらソフトウェアエンジニア失格。

cl-libを使わなかったのでループが辛いのもさることながら、cl-labelsとかcl-fletなどの一時的な関数定義が使えないのが可読性の低さに拍車をかけている気がします。またリスト操作関数がmapcarぐらいしかないのがEmacs Lispの辛いところ。

またLispの常ですが、処理の順番とコードの順番が一致しないので、処理内容を把握するのにひと手間かかります。これを解消するには[dash.el](https://github.com/magnars/dash.el)のスレッディングマクロをぜひ使いたいところです。折しもつい最近[るびきちさんが紹介しています](http://rubikitch.com/2015/05/30/dash-threading-macro/)。

例えば問題4をdash（とs）を使って書き換えると以下のようになります。

```emacs-lisp
(require 'dash)
(require 's)
(defun largest-combined-number2 (lst)
  (->> lst                              ; リストの
       (-map #'number-to-string)        ; 各要素を文字列に変換して
       (--sort (string< other it))      ; 逆順にソートして
       (s-join "")                      ; 結合して
       (string-to-number)))             ; 数値に変換する
(largest-combined-number2 '(50 2 1 9))  ; => 95021
```

上から下に読んでいけばそのまま処理を追っていけるので、可読性が抜群に高いです。豊富なリスト操作関数も含めて、dashおすすめです。


### 結論 {#結論}

cl-libやdashを積極的に使っていきましょう。