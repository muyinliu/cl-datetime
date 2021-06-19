# cl-datetime

cl-datetime is a Common Lisp feature to deal with date and time.

## License

Copyright (c) 2017 Muyinliu Xing
Released under the ISC License.

## Compatibility

|  Common Lisp  |  Linux  |  Mac |  Unix | Windows |
|---------------|:-------:|:----:|:-----:|:-------:|
|  SBCL         |   Yes   |  Yes |       |   Yes   |

Note: I don't have Unix system so haven't test on Unix yet.

Note: Have test in Windows XP/Windows 7/Windows 8.1/Windows 10

Note: Welcome to reply test results in other Common Lisp implements.

## Install and load with QuickLisp

In shell:

```shell
git clone https://github.com/muyinliu/cl-datetime.git
cp -r cl-datetime ~/quicklisp/local-projects/
```

Then in Common Lisp:

```lisp
(ql:quickload 'cl-datetime)
```

Note: nicknames of `cl-datetime`: `datetime`, `dt`

## Usage

### get universal-time in millisecond(ms) or microseconds(us)

```lisp
(dt:get-universal-time-ms)
```
=>
```=>
3708816107292
```

```lisp
(dt:get-universal-time-us)
```
=>
```=>
3708816107292184
```

### get unix-time in second, millisecond(ms) or microseconds(us)

```lisp
(dt:get-unix-time)
```
=>
```=>
1499827414
```

```lisp
(dt:get-unix-time-ms)
```
=>
```=>
1499827414707
```

```lisp
(dt:get-unix-time-us)
```
=>
```=>
1499827414707130
```

### convert between universal-time and unix-time

#### convert universal-time to unix-time

```lisp
(dt:universal-time->unix-time (get-universal-time))
```
=>
```=>
1499829423
```

```lisp
(dt:universal-time-ms->unix-time-ms (dt:get-universal-time-ms))
```
=>
```=>
1499829423882
```

```lisp
(dt:universal-time-us->unix-time-us (dt:get-universal-time-us))
```
=>
```=>
1499829423882339
```

#### convert unix-time to universal-time

```lisp
(dt:unix-time->universal-time 1499829423)
```
=>
```=>
3708818223
```

```lisp
(dt:unix-time-ms->universal-time-ms 1499829423882)
```
=>
```=>
3708818223882
```

```lisp
(dt:unix-time-us->universal-time-us 1499829423882339)
```
=>
```=>
3708818223882339
```

### format

```lisp
(format t (dt:datetime-formatter "yyyy-MM-dd'T'HH:mm:ss.SSSZ"))
```
=>
```=>
2017-07-12T10:41:47.292+0800
```

#### supported format string

| String  | Meaning                 | Output example | Note                          |
|---------|-------------------------|----------------|-------------------------------|
| `yyyy`  | year in 4 digit         | 2017           |                               |
| `yyy`   | year in 3 digit         | 017            |                               |
| `yy`    | year in 2 digit         | 17             |                               |
| `y`     | year in 1 digit         | 7              |                               |
| `MM`    | month in 2 digit        | 07             |                               |
| `M`     | month in 1 digit        | 7              |                               |
| `dd`    | day in 2 digit          | 14             |                               |
| `d`     | day in 1 digit          | 4              |                               |
| `HH`    | hour in 2 digit         | 17             |                               |
| `H`     | hour in 1 digit         | 7              |                               |
| `hh`    | hour in 2 digit(12hour) | 12             |                               |
| `h`     | hour in 1 digit(12hour) | 2              |                               |
| `mm`    | minute in 2 digit       | 35             |                               |
| `m`     | minute in 1 digit       | 5              |                               |
| `ss`    | second in 2 digit       | 48             |                               |
| `s`     | second in 1 digit       | 8              |                               |
| `SSS`   | millisecond in 3 digit  | 357            |                               |
| `SS`    | millisecond in 2 digit  | 35             |                               |
| `S`     | millisecond in 1 digit  | 3              |                               |
| `Z`     | timezone                | +0800          |                               |
| `a`     | AM/PM                   | AM             | hour < 12 is AM, otherwise PM |
| `EEEEE` | week days in 1 letter   | T              |                               |
| `EEEE`  | week days' fullname     | Thursday       |                               |
| `EEE`   | week days' shortname    | Thu            |                               |
| `EE`    | week days' shortname    | Thu            |                               |
| `E`     | week days' shortname    | Thu            |                               |

Note: week days now only support English.

Note: AM/PM now only support English.

#### format with timezone

```lisp
(format t 
        (dt:datetime-formatter "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
        :time-us (get-universal-time-us)
        :zone 0)
```
=>
```=>
2017-07-12T02:41:47.292+0000
```

```lisp
(format t 
        (dt:datetime-formatter "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
        :time-us 3708816107292184
        :zone -8)
```
=>
```=>
2017-07-12T10:41:47.292+0800
```

#### enable datetime formatter's readtable

```lisp
(named-readtables:in-readtable dt:datetime-readtable)
```

```lisp
(format t #_"yyyy-MM-dd'T'HH:mm:ss.SSSZ")
```
=>
```=>
2017-07-12T10:41:47.292+0800
```

Note: use `#_` as datetime formatter 's read macro

### parse

;; TODO not support yet

## Test cl-datetime

```shell
sbcl --noinform --eval "(asdf:test-system :cl-datetime)" --quit
```
