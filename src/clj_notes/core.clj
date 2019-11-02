(ns clj-notes.core
  (:gen-class)
  (:import (clojure.java.api Clojure)
           (java.util Date Random)))
;:gen-class generate java class file

;todo: trampolining( trampoline蹦床),尾递归优化 http://jakemccrary.com/blog/2010/12/06/trampolining-through-mutual-recursion/
;todo: STM
;todo: 多重方法defmulti/defmethod
;todo: modifiers

;Param 和 Arg的区别
;Parameter is variable in the declaration of function.
;Argument is the actual value of this variable that gets passed to function.

;!!! clojure中逗号等于空白符
;!!! everything but false and nil evaluates to true in Clojure

;install leiningen:
;put lein.bat in your PATH
;open cmder,run: lein repl
;start repl,use exit,(exit),(quit) or ctrl+d to quit repl
(println "hello clojure")

;Symbols are used to bind names to values
;' will prevent a form from being evaluated
;'() same as (quote ())

;def global variable
;let local variable binding
(def gv "light")
(println gv)

(let [x 10
      y 20
      z 30]
  (+ x y z))
;=> 60



;defn 定义函数
;defn- 定义ns内私有函数
(defn f
  "the second line is doc-string"
  {:added  "1.2"                                            ;this is attr-map
   :static true}
  [param]
  (print "hello " param))

(meta (var f))
;#' is the reader macro for var and works the exactly same
(meta #'f)

;fn create a function
(def f (fn [] (println "this is from fn function")))
;#() is the shortcut for fn
(def plus-one #(+ 1 %))
;% will be replaced with arguments passed to the function
;%1 is for the first argument, %2 is for the second and so on



;;;序列(sequence) 是clojure中重要的概念，序列包含三个重要特性方法：
;(first coll),(next coll),(cons item seq),
;一般还包括(rest coll),(more coll)两个方法(这些方法定义在clojure.lang.ISeq接口中)
;可以生成序列的结构称为 seqable
(def coll [])
(let [s (seq coll)]
  (if s
    (comment "work on (first s) and recur on (rest s)")
    (comment "all done - terminate")))
;Sequence functions (map, filter, etc)implicitly call seq on the incoming (seqable) collection and
;return a sequence (possibly empty, not nil)


;几乎一切数据结构在clojure中都是序列，这些数据结构包括：
;All iterable types (types that implement java.util.Iterable)
;Java collections (java.util.Set, java.util.List, etc)
;Java arrays
;Java maps
;All types that implement java.lang.CharSequence interface, including Java strings
;nil
;clojure.lang.ISeq - the sequence abstraction interface,更常用的是clojure.lang.ASeq,clojure.lang.LazySeq
;clojure.lang.Seqable - seqable marker,只有一个方法:ISeq seq();
;clojure.lang.Sequential - 遍历顺序和初始化顺序一致,Lists, vectors, and effectively all seqs are sequential.

;clojure中4大数据结构,list是直接实现ISeq接口,而set,vector,map实现的是Seqable接口
(seq nil)
;;=> nil
(seq ())
;;=> nil
(sequence ())
;;=> ()
(sequence nil)                                              ;(sequence nil) yields ()
;;=> ()


;;创建seq的函数
;range,repeat,iterate,cycle,interleave(交错取值),interpose(给序列插入一个间隔值)

;过滤序列
(filter even? coll)
(take-while even? coll)
(drop-while even? coll)
(split-with even? coll)
;assoc-in associate使加入
;split-at,take-,drop-
;every?,some,not-every?,not-any?,

;序列转换
;map,reduce,sort,sort-by,
;(file-seq)
;(for)

;vector fn: conj nth count .indexOf
;user=> (.indexOf [1 2 3] 4)
;-1

;user=> (count [1 2])
;2

;set fn: conj nth count disj sort contains? subset? superset?
#{1 2 3}

;map fn: assoc merge keys vals
(let [os {:Apple "Mac" :Microsoft "Windows"}]
  (get os :Apple))

(assoc {:Apple "Mac" :Microsoft "Windows"} :Commodore "Amiga")


;function and destructuring example
(defn des
  "args means get :k1 value from argument (map) and binding it to k1(parameter)"
  [{k1 :k1}]
  (println "destructing in map" k1))

;also you can destructuring from str/symbol key
(defn currency-of
  [{currency "currency"}]
  currency)
(defn currency-of
  [{currency 'currency}]
  currency)
;可以用:keys,:strs,:syms一次性解构所有参数,形参和实参同名
(defn currency-of
  [{:keys [currency amount]}]
  (println currency amount))

(currency-of {:currency "RMB" :amount 100000})              ;ok
(currency-of {"currency" "RMB" "amount" 100000})            ;currency will be nil,you will need use :strs or :syms

;:strs
(defn currency-strs
  [{:strs [currency amount]}]
  currency)
(currency-strs {"currency" "RMB" "amount" 100000})          ;ok

;:syms
(defn currency-syms
  [{:syms [currency amount]}]
  currency)
(currency-syms {'currency "CNY" 'amount 100000})            ;ok

;默认值参数 use :or to give a default value for parameter
(defn currency-or
  [{:keys [currency amount] :or {currency "USD"}}]
  currency)
(currency-or {:amount 100000})                              ;=> "USD"

;不定长参数 use & for Variadic Functions parameters
(defn log
  "first arg is msg and other arguments are stored in args,args is a seq"
  [msg & args]
  (println "msg=" msg ",and rest args=" (map #(str "*" %1 "*") args)))

;user=> (log "hi" "jim" "bella")
;msg= hi ,and rest args= (*jim* *bella*)

;命名参数 named params , achieved by Variadic Functions parameters destructing
(defn job-info
  [& {:keys [name job income] :or {job "unemployed" income "$0.00"}}]
  (if name
    [name job income]
    (println "No name specified")))

;cation! 这里实参不是map
(job-info :name "Robert" :job "Engineer")
;=> ["Robert" "Engineer" "$0.00"]

;Without the use of a variadic argument list,
;you would have to call the function with a single map argument such as
(defn job-info-map
  [{:keys [name job income] :or {job "unemployed" income "$0.00"}}]
  (if name
    [name job income]
    (println "No name specified")))

(job-info-map {:name "Robert" :job "Engineer"})
;=> ["Robert" "Engineer" "$0.00"]

; more example on destructing: https://gist.github.com/john2x/e1dca953548bfdfb9844
(def my-vec [1 2 3])

(let [[a b c d] my-vec]
  (println a b c d))
;1 2 3 nil

(let [[a b & the-rest] my-vec]
  (println "a=" a "b=" b "the-rest=" the-rest))
;a= 1 b= 2 the-rest= (3)
(let [[:as all] my-vec]
  (println all))
;[1 2 3]`
(let [[a :as all] my-vec]
  (println a all))
;1 [1 2 3]
(let [[a b & the-rest :as all] my-vec]
  (println a b the-rest all))
;1 2 (3) [1 2 3]
;!!! note: & the-rest convert vector to list,
;but :as preserves them (as a list, or as a vector)

(def my-vec ["first" "second"])
(let [{a 0 b 1} my-vec]
  (println a b))                                            ;=> "first second"

;optional arguments to functions
(defn foo [a b & more-args]
  (println a b more-args))
(foo :a :b)                                                 ;; => :a :b nil
(foo :a :b :x)                                              ;; => :a :b (:x)
(foo :a :b :x :y :z)                                        ;; => :a :b (:x :y :z)

;map destructuring
(def my-hashmap {:a "A" :b "B" :c "C" :d "D"})
(def my-nested-hashmap {:a "A" :b "B" :c "C" :d "D" :q {:x "X" :y "Y" :z "Z"}})

(let [{a :a d :d} my-hashmap]
  (println a d))
;; => A D

(let [{a :a, b :b, {x :x, y :y} :q} my-nested-hashmap]
  (println a b x y))
;; => A B X Y
(let [{a :a, b :b, not-found :not-found, :or {not-found ":)"}, :as all} my-hashmap]
  (println a b not-found all))
;; => A B :) {:a A :b B :c C :d D}

;!!! There is no & rest func for maps.

;count file lines
(defn- num-lines
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (count (line-seq rdr))))

;:as bind entire map to param
;See https://github.com/ring-clojure/ring/wiki/File-Uploads for explanation
(defn file-handler
  "argument is a map,:as request binding the arg to request var,而
  {{{tempfile :tempfile filename :filename} \"file\"} :params  是嵌套解构,即从实参取出:params
  然后又从:params内部取出:filename和:tempfile,其中:tempfile是一个java.io.File"
  ;[{{{tempfile :tempfile filename :filename} "file"} :params :as request}]
  [{{{tempfile :tempfile filename :filename} "file"} :params :as request}]
  (println request)
  (let [n (num-lines tempfile)]
    (println (str "File " filename " has " n " lines "))))

;请求示例:
;{...
; :params
;  {"file" {:filename     "words.txt"
;           :content-type "text/plain"
;           :tempfile     #object[java.io.File ...]
;           :size         51}}
; ...}

;又一个嵌套解构的例子
(defn first-first
  "最外面的[]是表示参数,[[i _] _]表示实参必须一个二维以上的vector,只取第二维的第一个
  如果是1维,返回nil"
  [[[i _] _]]
  i)
;user=> (first-first [[1 2] [3 4]])
;1
;user=> (first-first [[[1 2] [3 4]] [5 6]])
;[1 2]

;(defn name doc-string? attr-map? [params*] prepost-map? body)
;(defn name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?)
;function can have params type hint
(defn round
  "^double here is type hint
  everything start with ^ means metadata"
  [^double d ^long precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/floor (* d factor)) factor)))


;重载函数
(defn bar
  "参数个数不同的重载,clojure还有更强大的defmulti/defmethod"
  ([a b] (bar a b 100))
  ([a b c] (* a b c)))

;复习上面学过的命名参数
(defn named-arg-fn [& {:keys [function sequence]}]
  (map function sequence))
;the arg is not a map!!!
;(named-arg-fn :sequence [1 2 3] :function #(+ % 2))


;namespace
;create-ns create a namespace
;in-ns move to a namespace
;require loads a namespace and
;refer refers the namespace.
;To do these at once, you can use use

;you can rename namespace
;(require '[clj-notes.core :as temp-ns])

;ns macro creates a new namespace and
; gives you an opportunity to load other namespaces at the creation time



;if
(if true
  (println "executed when true")
  (println "executed when false"))

;use do to execute multi expressions
(if true
  (do
    (println "one")
    (println "two")))

;if-let:
(defn positive-number [numbers]
  (if-let [pos-nums (not-empty (filter pos? numbers))]
    pos-nums
    "no positive numbers"))

;when when-let case cond condp
(defn cond-test
  [n]
  (cond
    (= n 1) "n is 1"
    (and (> n 3) (< n 10)) "n is over 3 and under 10"
    :else "n is other"))

(cond-test 1000)

;str
(let [first "Hirokuni"
      last "Kim"]
  (str "My name is " first " " last))

;format
(format "My name is %s %s" "Hirokuni" "Kim")

;power function
(defn power
  [x n]
  (reduce * (repeat n x)))

;bigint,N is a literal for bigint,M is BigDecimal
;user=> (+ 9223372036854775807 10N)
;9223372036854775817N
;user=> (+ 98765431123456789.1 10M)
;9.87654311234568E16

;for compression
(for [x '(1 2 3)]
  (+ 10 x))

;(doc for)
;双重for 循环
(for [x (range 10)
      y (range 20)
      :while (< y x)]
  [x y])

;<==> {x | x >0}
(for [x '(-1 1 2)
      :when (> x 0)]
  x)

(for [x [0 1 2 3 4 5]
      :let [y (* x 3)]
      :when (even? y)]
  y)



;# 的规则和用途===start
;# is Dispatch character that tells the Clojure reader how to interpret the next character
; using a read table
;set
#{1 2 3}

;discard two following input
;忽略:b 2
{:a 1, #_#_:b 2, :c 3}
;user=> {:a 1  #_ :b :c 2 :d 3}
;{:a 1, :c 2, :d 3}

;regular expression
(re-matches #"^test$" "test")
;=> "test"

;anonymous function
#(println %)
;var quote
(read-string "#'foo")
;symbolic values
(/ 1.0 0.0)
;#Inf

;tagged literals
(type #inst "2014-05-19T19:12:37.925-00:00")                ;java.util.Date
;meta
(defn fn-name
  []
  "hello")
(meta #'fn-name)

;reader conditionals 
;#?(:clj     (Clojure true)
;   :cljs    (ClojureScript false)
;   :default (fallthrough false))

;#?@ splicing reader conditional
;(defn build-list []
;  (list #?@(:clj  [5 6 7 8]
;            :cljs [1 2 3 4])))                              ;return [5 6 7 8] when run on clojure

;#= allows the reader to evaluate an arbitrary form during read time
(read-string "#=(+ 3 4)")                                   ;7
;# 的规则和用途===nd



;;;Recursion
;simple recursion
(defn fibo
  "this is recursion function"
  [n]
  (if (or (= n 0) (= n 1))
    n
    (+ (fibo (- n 1)) (fibo (- n 2)))))
;do not do this!!! take a long time to finish
;(fibo 1000)

;use recur
(defn fibo-recur [iteration]
  (let [fibo (fn [one two n]
               (if (= iteration n)
                 one
                 (recur two (+ one two) (inc n))))]
    ;recur re-binds it's arguments to new values and call the function with the new values
    ;fibo is an inner function
    (fibo 0N 1N 0)))

(fibo-recur 1000)
;it is really fast
;notes
;with simple recursion, each recursive call creates a stack frame which is 
;a data to store the information of the called function on memory.
;Doing deep recursion requires large memory for stack frames, but since it cannot, 
;we get StackOverflowError
;尾递归
;A function is tail recursive when the recursion is happening at the end of it's definition
;In other words, a tail recursive function must return itself as it's returned value.
;When you use recur, it makes sure you are doing tail recursion

;(doc loop)
;loop/recur is merely a friendly way to write recursion code.
;All imperative loops can be converted to recursions and all recursions can be converted to loops,
;so Clojure chose recursions.
;Although you can write code that looks like an imperative loop with loop/recur,
;Clojure is doing recursion under the hood.

;调用Java
;import java class
(import Date)
(println (str (new Date)))
;Wed Jul 24 22:55:24 CST 2019

(new Date "2016/2/19")
(Date.)
(Date. "2016/2/19")
(Math/pow 2 3)                                              ;static method
(def rnd (new Random))
(. rnd nextInt 10)

(let [date1 (new Date)
      date2 (new Date)]
  (.equals date1 date2))



;;;Java方法调用
;import
;(import & import-symbols-or-lists)
(import 'java.util.Date 'java.text.SimpleDateFormat)
(import '[java.util Date Set])
(ns clj-notes.core
  (:import (java.util Set Date Calendar TimeZone)
           (java.util.concurrent ConcurrentHashMap)))

;new a instance
(def sdf (new SimpleDateFormat "yyyy-MM-dd"))
;or
(def sdf-dot (SimpleDateFormat. "yyyy-MM-dd"))

;call instance method
(.parse sdf date-string)
;or use dot-form, see more on below
(. sdf parse "2019-10-03")

;access static method/field
;(ClassName/staticMethod args*)
;(ClassName/staticField)

;nested class
(.getEnclosingClass java.util.Map$Entry)
;-> java.util.Map

;dot special form
;all above java call style are expanded into calls to the dot operator (described below) at macroexpansion time.
; The expansions are as follows
(.instanceMember instance args*)
;==> (. instance instanceMember args*)
(.instanceMember ClassName args*)
;==>(. (identity ClassName) instanceMember args*)
(.-instanceField instance)
;==> (. instance -instanceField)
(ClassName/staticMethod args*)
;==> (. ClassName staticMethod args*)
(ClassName/staticField)
;==> (. ClassName staticField)

; general forms, dot read as 'in the scope of'
;(. ClassName member)
;(. instanceExpr member)
; if the member is start with "-", it will resolve only as field access,不会被识别为方法


;dot-dot
(import 'java.util.Calendar)
(. (. (Calendar/getInstance) getTimeZone) getDisplayName)
;use .. macro chain the calls
(.. (Calendar/getInstance)
    getTimeZone
    (getDisplayName true TimeZone/SHORT))

; doto macro
(doto calendar-obj
  (.set Calendar/AM_PM Calendar/AM)
  (.set Calendar/HOUR 0)
  (.set Calendar/MINUTE 0)
  (.set Calendar/SECOND 0)
  (.set Calendar/MILLISECOND 0))


; (memfn methodNm) 用于提示参数有这个methodNm
(map (memfn getBytes) ["amit" "rob" "kyle"])
;==
(map #(.getBytes %) ["amit" "rob" "kyle"])
; memfn 使用了反射,所以如果能够添加一个类型提示可以提高性能
(time (dotimes [n 100000] (mapv (memfn toLowerCase) ["A" "B" "C"])))
;"Elapsed time: 1188.276915 msecs"
(time (dotimes [n 100000] (mapv (memfn ^String toLowerCase) ["A" "B" "C"])))
;"Elapsed time: 74.903093 msecs"
; see the source of memfn

;bean convert java bean to map
(bean (Calendar/getInstance))


;;; java array
(def tokens (.split "clojure.in.action" "\\."))
;#'user/tokens
;user=> (type tokens)
;[Ljava.lang.String;
; alength,aget,aset function for java array, caution! java array is mutable







;;;宏
;clojure 的一半威力来自于macro,编写程序使用的是语言自带能力,编写macro则是扩展编程语言自身,例如语法defn
;什么时候使用macro? 规则1:你不需要macro;规则2:当你脑子里反复出现要是clojure要有X特性就好了,那说明你需要用macro实现这个X特性.

;第一个macro: unless函数只有在test==false时,才执行then表达式(例如then是print语句),
;如果unless是一个普通函数,那么then表达式在test执行的同时也会执行
;使用macro可以让then在指定的地方执行
(defmacro unless [test then]
  "Evaluates then expr when test evaluates to be false"
  (list 'if test nil then))

;一个macro有2步:先macro展开,然后编译.
;仔细思考其实macro和web编程中的模板技术非常相似,例如freemarker.
;freemarker中包含html所有的标签,同时增加了占位符概念,通过渲染时(macro展开)将占位符替换为实参.
;clojure提供了2种方式构建模板
;一种是通过list/concat等函数构建, 此时需要使用`quoting告诉list哪些是clojure原语(if,nil等)
;第二种是完全freemarker的模板语法,稍后介绍,先看一下macro展开工具
(macroexpand '(unless (blank? "s") (println "test arg is false")))
(macroexpand-1 '(unless false (println "test arg is false")))
;可以使用macroexpand-1和macroexpand查看macro展开形式,注意!!!macro展开形式是无法发现编译错误的.

;macro是一个野兽,到处都是你想象不到的陷阱,假设如下一个macro
(defmacro bad-unless [test then]
  (list 'if 'test nil then))

(macroexpand '(bad-unless (blank? "s") (println "should print this when false")))
;#>(if test nil (println "should print this when false"))
;碰巧 test是clojure的原语函数, (if test nil 1 2 3)始终 返回nil,
;如果你将参数test换成其名字,那么编译期间会报错,如下
;user=> (defmacro bad-unless [expr then]
;  #_=>   (list 'if 'expr nil then))
;#'user/bad-unless

;user=> (macroexpand '(bad-unless (blank? "s") (println "should print this when false")))
;(if expr nil (println "should print this when false"))

;user=> (bad-unless (blank? "s") (println "should print this when false"))
;Syntax error compiling at (REPL:1:1).
;Unable to resolve symbol: expr in this context

;所以不要随意使用函数名作为参数名!!!

; 注意list函数和'()构建list的区别: (list & items)中items会被evaluated,而'(& items) 不会.
(let [x 1 y 2]
  (list x y))
;; => (1 2)
;and when using quote ' they are not:
(let [x 1 y 2]
  '(x y))
;; => (x y)
;所以上面的list不能使用'替换
(defmacro unless [test then]
  '(if test nil then))
;user=> (macroexpand '(unless nil "ops"))
;((quote if) test nil then) ;test和then没有被展开为nil "ops", 'if中的quote也被保留

;使用list很快就会使得你无法看清macro最后生成的代码长啥样,仔细想想macro其实就是模板技术,例如freemarker
;定义好html标签和变量占位符,然后用实际变量替换占位符,clojure提供了相应的模板技术: `和~/~@
;` 表示一个模板的开始, ~/~@表示在模板内的占位符, ~@表示变量是一个list,转为不定长参数

;' quoting vs ` syntax-quoting
;1. ` returns the fully qualified namespace, and this is important
;2. ` allow unquote/splicing unquote in it

`(+ ~(list 1 2 3))
;(clojure.core/+ (1 2 3))

`(+ ~@(list 1 2 3))
;(clojure.core/+ 1 2 3)
;~@ The splicing unquote works just like ~ unquote,
;except it expands a sequence and
; splice the sequence contents into the enclosing syntax-quoted data structure
;是不是和java/scala里面的flatMap函数相似?

;remove namespace in `,use ~'expr,即在模板中使用~'普通引述
;user=> `[:a ~(+ 1 1) ~'c]
;[:a 2 c]
;user=> `[:a ~(+ 1 1) ~`c]
;[:a 2 user/c]

;到此为止,macro已经不可控了,不信你试试:
;user=> `[:a ~(+ 1 1) ~'c]
;user=> `[:a ~(+ 1 1) ~`c]
;user=> `{:a 1 :b '~(+ 1 2)}
;user=> `[:a ~(+ 1 1) '~'c]
;user=> `{:a 1 :b '~@(list 1 2)}
;user=> `(1 `(2 3) 4)
;user=> `(list 1 `(2 ~(- 9 6)) 4)


;看一下clojure自带的一些优雅的macro
(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro ..
  "form => fieldName-symbol or (instanceMethodName-symbol args*)

  Expands into a member access (.) of the first member on the first
  argument, followed by the next member on the result, etc. For
  instance:

  (.. System (getProperties) (get \"os.name\"))

  expands to:

  (. (. System (getProperties)) (get \"os.name\"))

  but is easier to write, read, and understand."
  {:added "1.0"}
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))


;thread first macro, here the thread means pipe
(-> []
    (conj 1)
    (conj 2)
    (conj 3))
;[1 2 3]

(first (.split (.replace (.toUpperCase "a b c d") "A" "X") " "))
;"X"

;;Perhaps easier to read:
;-> 后面是初始参数,第2行开始每一行是一个函数调用,
;且上一行的返回值会作为这一行第一个参数(这就是thread first)的first含义
;这里的thread是管道的意思,而不是并发编程的线程
;如果省略(),那么野生符号(bare symbol)和keyword都会当作一个函数调用,
;例如,这里的.toUpperCase是bare symbol,等效于(.toUpperCase args)
(-> "a b c d"
    .toUpperCase
    (.replace "A" "X")
    (.split " ")
    first)
;the previous call same as follow, comma is equals whitespace
(-> "a b c d"
    (.toUpperCase)
    (.replace "A" "X")
    (.split " ")
    first)

;suppose a function
(defn calculate []
  (reduce + (map #(* % %) (filter odd? (range 10)))))

;same as
;上一行的结果作为最后一个参数插入,这叫thread last
(defn calculate* []
  (->> (range 10)
       (filter odd?)
       (map #(* % %))
       (reduce +)))

;如果想要指定每次插入的位置那么需要用 as->
;v是每一行的返回值的名称,这样你可以在下一行任意参数位置指定
(as-> [:foo :bar] v
      (map name v)
      (first v)
      (.substring v 1))






;;;并发编程

;;;future and deref
(let [future-val (future (inc 1))]
  (println (deref future-val)))
;deref == @
(let [future-val (future (inc 1))]
  (println @future-val))

(def my-future (future (Thread/sleep 5000)))
(repeatedly 6
            (fn []
              (println (realized? my-future))
              (Thread/sleep 1000)))

;(doc future)

;promise
(def my-promise (promise))
;you define a promise
(def listen-and-callback (fn []
                           (println "Start listening...")
                           (future (println "Callback fired: " @my-promise))))

(defn do-time-consuming-job []
  (Thread/sleep 5000)
  (deliver my-promise "delivered value"))

(listen-and-callback)
(do-time-consuming-job)

;atom is like mutable var in other languages but atom is thread safe

;ref dosync ref-set alter
(def my-ref (ref 0))
(dosync
  (alter my-ref
         (fn [current_ref]
           (inc current_ref))))

(print @my-ref)

(def user (ref {}))
(dosync
  (alter user merge {:name "Kim"})
  (throw (Exception. "something wrong happens!"))
  (alter user merge {:age 32}))

(def user-record (atom {}))

(do (swap! user-record merge {:name "Kim"})
    (throw (Exception. "something wrong happens!"))
    (swap! user-record merge {:age 32}))

;;;并发编程




;todo: 补充oop中的多态知识点
;;clojure的多态和多重方法defmulti/defmethod
;java中的多态是通过方法重载来实现的,重载要求方法的参数个数或类型不同来实现. -- 静态分配
;clojure中实现参数个数重载非常简单,上面已经见过.
;clojure中的多态通过multi-method可以实现更为灵活的代码,clojure中一般不会用到多态,一旦用到,都是核心逻辑代码.
;假设一个场景:一个print函数,希望这个函数可以支持nil,string,vector,map等多种类型的打印,在Java中可以通过参数类型重载实现多个print方法.

(comment
  ;define a multi
  ;dispatch-fn can be keyword since when arg is map
  (defmulti multi-name docstring? attr-map? dispatch-fn & options)

  ;define a multi-method
  (defmethod multi-name dispatch-value & fn-tail)

  ;get the multimethod dispatch map
  (methods multi-name)
  ;or use get-method on arg to check which method is dispatched
  (get-method multi-name "mint.com")

  ;remove multimehtod impl by remove-method and removeall-methods.

  )

;todo: trampolining( trampoline蹦床),尾递归优化 http://jakemccrary.com/blog/2010/12/06/trampolining-through-mutual-recursion/
;蹦床运动解决的一个问题就是调用栈过深导致栈溢出.
(declare my-odd?)

(defn my-even? [n]
  (if (zero? n)
    true
    (my-odd? (dec (Math/abs n)))))

(defn my-odd? [n]
  (if (zero? n)
    false
    (my-even? (dec (Math/abs n)))))

;> (my-even? 10000)
;Execution error (StackOverflowError) at user/my-even? (REPL:4).
;这里就出现栈溢出,scala中同样有这个问题:
(comment
  "def foldR[A,B](as: List[A], b: B, f: (A,B) => B): B = as match {
    case Nil => b
    case h :: t => f(h,foldR(t,b,f))
  }
  "
  )

;clojure 通过 trampoline 函数解决这个问题

(defn my-even? [n]
  (if (zero? n)
    true
    #(my-odd? (dec (Math/abs n)))))

(defn my-odd? [n]
  (if (zero? n)
    false
    #(my-even? (dec (Math/abs n)))))

;> (trampoline my-even? 10000)

