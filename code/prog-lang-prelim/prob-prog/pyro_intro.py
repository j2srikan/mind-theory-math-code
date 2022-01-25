import torch
import pyro

print('hello world')

obs = pyro.sample('obs', pyro.distributions.Normal(torch.tensor(0.), torch.tensor(1.)))
print(obs)
