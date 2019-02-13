# Bayesian-Classifier
Identifies authorship of `new_text` using `autor_1` and `autor_2` texts.
Also you can save word frequencies of `autor_1` and `autor_2` into `base_1` and `base_2` files to use them again by following keys:
* To create frequency files execute `run_classifier.sh` with `create_base` key
* To use existing frequency files execute `run_classifier.sh` with `use_base` key
If you run the script without keys, source text will be used without creating base files. 
