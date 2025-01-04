# parse-argv

parse-argv is a library for Common Lisp that parses UNIX-style command line 
parameters and returns them as a list with type conversion according to rules.


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
Use [quicklisp](http://www.quicklisp.org/).

```
* (ql:quickload "parse-argv")
```
-->

　Use ASDF.

```
* (asdf:load-system "parse-argv")
```

The test code can be executed at:

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

Parses a list of command line parameters whose elements are strings 
according to the given rules. The result is a list of normalized values. 
Here, “list of normalized values” means a list in a format that allows 
`APPLY` to be called with a function corresponding to the rule.

The input parameters are passed as a list of strings to `args` , and 
the rules for parsing options and parameters are passed to `option-rules` 
and `param-rules` , respectively. `option-rules` are rules for option 
parsing. It should be a list whose elements are lists that can be 
distributed as follows:

```
(keyword flag &optional long-option type default-value)
```

The explanation of each element in the above rules is as follows:

*   `keyword` : Specify the parameter names to be used in the normalized 
    output with keyword symbols.
*   `flag` : Specify the character to be used for short option forms 
    such as `-a` by `CHARACTER` such as `#\a` .
*   `long-option` : If long option is supported, specify its name as a string.
*   `type` : To make an option with a value, specify its type with a keyword 
    symbol. You can use `:string :boolean :integer :keyword` . The type can be 
    added by adding a method to the generic function `convert-value` .
*   `default-value` : Specify the default value for a valued option.


Here is an example of `option-rules` .

```
(defparameter *option-rules*
  '((:extra #\x)
    (:all   #\a "--all")
    (:count #\c "--count" :integer 3)))
```

`:extra` is available with `-x` , and `:all` is available with `-a` or `--all` . 
Since these are not valued options, they are `T` if specified and `NIL` otherwise. 
In contrast, `:count` is a valued option with `:integer` , so it can be specified 
with `-c5` or `--count=9` . The default value is 3.

An example of actual use is shown below.

```
(argv:parse '("-all" "-x" "-c5") *option-rules* nil)
 => (:ALL T :EXTRA T :COUNT 5)
```

As you can see in the example above, `argv:parse` performs parsing similar to 
bash and other option processing.

*   Short options can be specified collectively. `-a -b` and `-ab` have the 
    same meaning.
*   If `-f` and `--foo` are different forms of the same option, then 
    `-f 2 / -f2 / --foo 2 / --foo=2` are all valid and have the same meaning 
    if this is a valued parameter.
*   Use `--` to explicitly end an option.
*   If `-a` does not take a value parameter and `-b` does, then `-ab234` is 
    equivalent to `-a -b 234` .
*   Long form options such as `--value` are valid in short form as long as 
    they are not ambiguous.


`param-rules` specifies rules for non-optional parameters. In its simplest 
form, it specifies the number and type of required parameters as follows:

```
(argv:parse '("string" "123" "foo") nil '(:string :integer :keyword))
 => ("string" 123 :FOO)
```

In the above specification, exactly three (non-optional) parameters must be 
specified, and it is an error to specify too few or too many. If you want to 
add some latitude to this, use `&optional` as follows

```
(argv:parse '("string" "123") nil
            '(:string &optional (:integer 0) (:keyword :FOO)))
 => ("string" 123 :FOO)
```

In the above, you can give 1 to 3 parameters. If you want to take an indefinite 
number of parameters, you can use `&rest` . The parameters corresponding to the 
`&rest` part will be recovered in the list as strings.

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

Convert `string-data` to a value of the appropriate type according to `type` 
and return it. However, if `string-data` is `NIL` , `default-value` is returned. 
The `parse-argv` library provides the following conversions.

*   `:string` : String type. Returns the value of `string-data` as is 
    (unless it is `NIL` ).
*   `:boolean` : Boolean type. Converts `“true”` and `“false”` to 
    `T/NIL` and returns them. Otherwise, an error is returned.
*   `:integer` : Integer type. Returns the result of applying `string-data` 
    (unless `NIL` ) to a `parse-integer` .
*   `:keyword` : Keyword symbol. Returns the result of `string-upcase` -ing 
    `string-data` (unless it is `NIL` ) and `intern` it to the `keyword` 
    package.


`convert-value` is a generic function. Additional type specifications can be 
added by adding methods. See the Appendix for details.


### argv:get-argv

```
argv:get-argv => result
```

* result ---- a list of string

Returns command line arguments in the form of a list of strings. 
This is just a compatibility layer to absorb differences among 
implementations, and the implementation is borrowed from `apply-argv` . 
See the implementation for details.

## Appendix

### Adding user-defined type conversion

To add user-defined type conversion, add a method to the generic function 
`argv:convert-value` . For example, to implement a generic numeric conversion 
using the `parse-number` library:

```
(ql:quickload :parse-number)

(defmethod argv:convert-value ((type (eql :number)) string-data default-val)
  (if (null string-data)
      default-val
      (parse-number:parse-number string-data :float-format 'double-float)))
```


This allows `:number` to be used in rules passed to `argv:parse-argv` . 
Here is an example:

```
(argv:parse '("3/14" "#c(3 14)" "3.14") nil '(:number :number :number))
=> (3/14 #C(3 14) 3.14d0)
```


You can also add `:any` using `read-from-string` as follows, but this is 
not recommended for several reasons:

```
(defmethod argv:convert-value ((type (eql :any)) string-data default-val)
  (if (null string-data)
      default-val
      (read-from-string string-data)))
```


First, when you want to pass a string, for example, additional double quotes 
are required. That is, `'--param=“string”'` and so on. Even if you can live 
with this, it is dangerous to use `read-from-string` to innocently pass user 
input. Even if you don't `eval` , you can use the read-syntax `#.` can be 
used to execute arbitrary code, even without `eval` . Therefore, it is not 
easy to implement `:any` in a safe way. Be very careful when adding `:any` .

