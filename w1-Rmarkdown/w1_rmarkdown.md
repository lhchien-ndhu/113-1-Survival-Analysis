---
title: 'Week1: Rmarkdown demo'
author: "Li-Hsin Chien"
date: "2024-09-07"
output: 
  html_document:
    keep_md: true
    #number_sections: true
    toc: true # 自動生成目錄
    toc_float: true #設定目錄的顯示方式為浮動側邊攔
    code_folding: hide #可折疊所有代碼塊
---

參考網站: https://cosname.github.io/rmarkdown-guide/ 

比較常會用到的網站內容為 2.6 (介紹 Markdown 語法)。

# 標題 1

這是標題一的內容，用 R 計算 $2^10$:


```r
2^10
```

```
## [1] 1024
```

## 標題 1.1

次標題內容

# 標題 2

用 R 計算:

$$
1 + 2 + \cdots + 100 = \sum_{i=1}^{100} i 
$$

```r
sum(1:100)
```

```
## [1] 5050
```




