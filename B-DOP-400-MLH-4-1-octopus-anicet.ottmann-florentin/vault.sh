#!/bin/bash

export ANSIBLE_VAULT_PASSWORD_FILE=/tmp/.vault_pass
echo verySecretPassword > /tmp/.vault_pass
ansible-vault encrypt group_vars/all.yml
ansible-playbook -i production playbook.yml
