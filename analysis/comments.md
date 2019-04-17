
理论上，不平衡样本不是大问题。
样本的平衡程度是影响逻辑回归的截距，但是截距一般是不重要的，因为可以剔除，因为在进行切bin时，我们选择`cut_number`的方式，均分样本，截距大小不影响切分(<span class="citeproc-not-found" data-reference-id="狗熊会正负样本分布不均衡">**???**</span>)。

但是实际工作中还是有影响，这里使用SMOTE算法。 SMOTE算法是由 Chawla et al. (2002) 剔除，这是一种过采样，over
sampling。
新增数据的方法是随机选择\(m\)个少类样本，找到靠近这\(m\)个样本的\(n\)个样本，随机选择这\(n\)个样本的其二，连线得到一个点，这个点就是新增样本(<span class="citeproc-not-found" data-reference-id="刘建平SMOTE">**???**</span>;
<span class="citeproc-not-found" data-reference-id="机器学习之旅smote">**???**</span>)。

``` r
install.packages(“DMwR”,dependencies=T)
library(DMwR)
#加载smote包
newdata=SMOTE(formula,data,perc.over=,perc.under=)
#formula:申明自变量因变量
#perc.over：过采样次数
#perc.under：欠采样次数
```

<div id="refs" class="references">

<div id="ref-Chawla2002SMOTE">

Chawla, Nitesh V., Kevin W. Bowyer, Lawrence O. Hall, and W. Philip
Kegelmeyer. 2002. “SMOTE: Synthetic Minority over-Sampling Technique.”
*Journal of Artificial Intelligence Research* 16 (1): 321–57.

</div>

</div>
