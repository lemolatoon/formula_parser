# formula_parser

論理式をパースしてくれます。

例）
|stdin|P -> Q|
|---|---|
|stdout|NdThen(NdLetter("P"), NdLetter("Q"))|

命題記号には大文字または小文字の一文字アルファベットをつかうことができます。

使える論理記号は、以下のものです。
```
! and or ->
```

また、補助記号としては `()`を使うことができます。

論理記号の結合優先順位は、
```
! > and | or > ->
```
です。詳しくは`bnf`を見てください。

# 実行
`git clone`して`cargo run`
