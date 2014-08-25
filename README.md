# Automated dotfiles

Here are my dotfiles and miscellaneous scripts, managed with ansible.

To run:

```sh
ansible-playbook -i hosts vagrant.yml
```

## What's included

* Emacs and my configs
* Tmux, git, etc
* Apache with virtual hosts configuration

## Warning

The whole playbook runs using sudo.

These ansible roles contain shortcuts for a development
environment. They are not designed for production use!
