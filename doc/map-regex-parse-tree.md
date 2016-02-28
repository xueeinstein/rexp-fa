# Map Regex parse tree elements to FA elements

* __:VOID__ map to the $\varepsilon$
* __:SEQUENCE__ map to the concatenation operation
* __:ALTERNATION__ map to the union operation
* __:CHAR-CLASS__, __:INVERTED-CHAR-CLASS__, with range like (__:RANGE__ #\a #\z) map to range operation
* (__:GREEDY-REPETITION__|__:NON-GREEDY-REPETITION__ &lt;min&gt; &lt;max&gt; &lt;parse-tree&gt;) map to the iteration operation
* __:REGISTER__, we can ignore it, because it always occurs with parentheses, such as '(0|1)*'
