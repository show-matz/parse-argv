# parse-argv

　parse-argv は Common Lisp 向けのライブラリで、UNIX スタイルのコマンドライン
パラメータをパースし、ルールに従って型変換したリストにして返します。


```
(argv:parse '("--all" "--mode=extra" "-c" "--" "123" "string")
            '((:all   #\a "--all")
              (:mode  #\m "--mode"  :keyword :NORMAL)
              (:count #\c "--count" :integer 3))
            '(:integer :string &optional (:keyword :foo)))

==> (123 "string" :FOO :ALL T :MODE :EXTRA :COUNT 3)
```



## Installation

<!--
　[quicklisp](http://www.quicklisp.org/) を使用してください。

```
* (ql:quickload "parse-argv")
```
-->

　ASDF を使用してください。

```
* (asdf:load-system "parse-argv")
```

　テストコードは以下で実行できます。

```
* (asdf:perform :test-op (asdf:find-system :parse-argv))
```

## Functions

### argv:parse

```
argv:parse args option-rules param-rules => result
```

* args ---- a list of string
* option-rules ---- a list of option-rule
* param-rules ---- a list of param-rule
* result ---- a list of values

　与えられたルールに従って、文字列を要素とするコマンドラインパラメータのリストを
パースします。結果として、正規化された値のリストを返します。ここで「正規化された
値のリスト」とは、ルールに対応する関数とともに `APPLY` をコールできるような形式の
リストであることを意味します。

　入力となるパラメータ群は文字列のリストとして `args` に、オプションやパラメータ
を解析するためのルールは `option-rules` および `param-rules` にそれぞれ渡します。 
`option-rules` はオプション解析のためのルールです。これは、以下のように分配できるリストを
要素とするリストである必要があります。

```
(keyword flag &optional long-option type default-value)
```

　上記ルールにおける各要素の説明は以下の通りです。

*   `keyword` : 正規化された出力で使用するパラメータ名をキーワードシンボルで指定します。
*   `flag` : `-a` などの short option 形式に使用する文字を `#\a` などの `CHARACTER` で
    指定します。
*   `long-option` : long option をサポートする場合、その名前を文字列で指定します。
*   `type` : 値付きのオプションにする場合、その型をキーワードシンボルで指定します。 
    `:string :boolean :integer :keyword` が使用できます。総称関数 `convert-value` に
    メソッドを追加することで型を追加することができます。
*   `default-value` : 値付きオプションのデフォルト値を指定する場合、その値を指定します。


　以下に例を示します。

```
(defparameter *option-rules*
  '((:extra #\x)
    (:all   #\a "--all")
    (:count #\c "--count" :integer 3)))
```

　`:extra` は `-x` で、 `:all` は `-a` または `--all` で利用できます。これらは値付きの
オプションではないため、指定されれば `T` 、されなければ `NIL` となります。これに対して 
`:count` は `:integer` が指定された値付きオプションのため、 `-c5` や `--count=9` などの
指定が可能です。また、デフォルト値として 3 が指定されています。

　実際の使用例を以下に示します。

```
(argv:parse '("-all" "-x" "-c5") *option-rules* nil)
 => (:ALL T :EXTRA T :COUNT 5)
```

　以上からもわかる通り、おおむね bash などのオプション処理と同様の解析を行ないます。

*   short option はまとめて指定できます。 `-a -b` と `-ab` は同じ意味になります。
*   `-f` と `--foo` が同じオプションの異なる形式とした場合、これが値付きパラメータで
    あれば `-f 2 / -f2 / --foo 2 / --foo=2` は全て有効であり、同じ意味になります。
*   `--` でオプションの終了を明示します。
*   `-a` が値パラメータを取らず `-b` が取るならば、 `-ab234` は `-a -b 234` と同じ
    意味になります。
*   `--value` などの長い形式のオプションは、曖昧にならない限り短く書いても有効です。


　`param-rules` では、オプションではないパラメータに関するルールを規定します。もっとも
単純な形式としては、以下のように必須パラメータの数と型を指定します。

```
(argv:parse '("string" "123" "foo") nil '(:string :integer :keyword))
 => ("string" 123 :FOO)
```

　上記のような指定では、（オプションでない）パラメータはきっかり３つ指定する必要があり、
少なくても多くてもエラーになります。これに幅を持たせたい場合、以下のように `&optional` 
を使用します。

```
(argv:parse '("string" "123") nil
            '(:string &optional (:integer 0) (:keyword :FOO)))
 => ("string" 123 :FOO)
```

<!-- ToDo : &optional なのにリストじゃない場合には対応したけど、どこまで書く？ -->

　上記であれば、パラメータは１〜３個を与えることができます。不定数のパラメータを取り
たい場合は、 `&rest` を使うことができます。 `&rest` 部分に該当するパラメータは
文字列のままリストに回収されます。

```
(argv:parse '("123" "string" "456") nil '(:integer &rest))
 => (123 ("string" "456"))
```



### argv:convert-value

```
convert-value type string-data default-value => result
```

* type ---- a keyword symbol
* string-data ---- a string
* default-value ---- a value
* result ---- a value

　`type` に応じて `string-data` を適切な型の値に変換して返します。ただし、 
`string-data` が `NIL` の場合、 `default-value` を返します。 `parse-argv` 
ライブラリは以下の変換を用意しています。

* `:string` : 文字列型です。 `string-data` の値を（ `NIL` でない限り）そのまま返します。
* `:boolean` : 真偽値型です。`"true"` および `"false"` を `T/NIL` に変換して返します。それ以外の場合はエラーにします。
* `:integer` : 整数型です。 `string-data` を（ `NIL` でない限り） `parse-integer` に適用した結果を返します。
* `:keyword` : キーワードシンボルです。 `string-data` を（それが `NIL` でない限り） `string-upcase` して `keyword` パッケージに `intern` した結果を返します。

　`convert-value` は総称関数です。メソッドを追加することにより、型指定を追加する
ことができます。詳細は Appendix を参照してください。

### argv:get-argv

```
argv:get-argv => result
```

* result ---- a list of string

　コマンドライン引数を文字列のリスト形式で返します。これは処理系ごとの差異を吸収
するための互換性レイヤーに過ぎず、実装も `apply-argv` から拝借したものです。
詳細は実装を参照してください。

## Appendix

### Adding user-defined type conversion

　ユーザー定義の型変換を追加するには、総称関数 `argv:convert-value` にメソッドを
追加します。たとえば、 `parse-number` ライブラリを使用した汎用的な数値変換を実装
するには以下のようにします。

```
(ql:quickload :parse-number)

(defmethod argv:convert-value ((type (eql :number)) string-data default-val)
  (if (null string-data)
	  default-val
	  (parse-number:parse-number string-data :float-format 'double-float)))
```

　これにより、 `argv:parse-argv` に渡すルールにおいて `:number` を使用できる
ようになります。以下に例を示します。

```
(argv:parse '("3/14" "#c(3 14)" "3.14") nil '(:number :number :number))
=> (3/14 #C(3 14) 3.14d0)
```


　また、以下のようにすれば `read-from-string` を使用した `:any` を追加できますが、
これはいくつかの理由でお勧めしません。

```
(defmethod argv:convert-value ((type (eql :any)) string-data default-val)
  (if (null string-data)
	  default-val
	  (read-from-string string-data)))
```

　まず、文字列を渡したい時にたとえば追加のダブルクオートが必要になります。
つまり、 `'--param="string"'` などとしなければなりません。これを我慢できたと
しても、ユーザー入力を無邪気に `read-from-string` するのはそもそも危険です。 
`eval` しない前提でも、リード構文 `#.` を使えば任意のコードを実行できるから
です。そのため、安全なやり方で `:any` を実装するのは簡単なことではありません。
追加する場合は十分に注意してください。

