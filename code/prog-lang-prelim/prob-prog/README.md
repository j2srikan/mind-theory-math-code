# Pyro

Group: Renee Leung, Julia Schirmeister, Alexander Walker

We choose the Python-based Pyro as our probabilistic programming language of choice. This choice was made on account of our group members either having some familiarity with Python (Julia, Renee) or being in the midst of learning Python (Alex).


## Key features of Pyro
Pyro is a probabilistic programming language (PPL) based on Python and PyTorch. It is designed to unify the language of probability with the deep learning framework to build models for reasoning and decision-making in AI and machine learning. Probabilistic models incorporate random variables and probability distributions into the model, which allows the model to express dependencies between random variables and output any posterior distribution instead of a specific value.


Strengths:
- Can make inferences on cause-effect relationships using Bayes rule and reason under uncertainty
- Can automate backward program-running and make use of GPUs to improve efficiency and scalability

Weaknesses:
- Shares weaknesses of Python (e.g. dynamic compilation may be error-prone) and PyTorch (e.g. less support for a relatively new language)


## Installation
You can install Pyro as a Python package. You could first (optionally) create a virtual environment to install pip packages on.
Run the following in the terminal:
```
python3 -m venv myenv # create virtual env
source myenv/bin/activate # activate virtual env
pip install --upgrade pip
pip install torch
pip install pyro-ppl
```

You can also run Pyro in your browser without needing to install anything on your computer using Google Colabs (https://colab.research.google.com). To do so you would run the following code in your Google Colab notebook (the exclamation mark in the front means to run the code block as a shell command):
```
!pip install torch
!pip install pyro-ppl
```

## Toy example
The following runs a basic program that prints “Hello world.” This program also prints a variable whose value is sampled from the standard normal distribution:
```
import torch
import pyro

print('Hello world')

x = pyro.sample('obs', pyro.distributions.Normal(torch.tensor(0.), torch.tensor(1.)))
print(x)
```

If running a Python source file (e.g. pyro_intro.py), run the following in the terminal:
```
python pyro_intro.py
```
