# 式 `(magnitude z)` を評価した際に呼ばれるすべての手続きのトレース

`magnitude`
↓
`apply-generic`
↓
`'magnitude` （`'(complex)`）
↓
`magnitude`
↓
`apply-generic`
↓
`'magnitude` （`'(rectangular)`）

のように動く。図 2.24 の複素数は直行形式の複素数として表現されており、直行形式の複素数に関する `magnitude` はすでに登録されている。そのため、複素数一般に関する `magnitude` を登録すれば、`complex` のタグを外したあと登録済みの `rectangular` に対する `magnitude` が呼び出される。
