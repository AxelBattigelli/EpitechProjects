# Octopus Project: VM Creation and Deployment with Ansible

This project involves creating 5 Virtual Machines (VMs) and deploying them using Ansible.

## Prerequisites
- Ensure that you have `ansible` and `ansible-vault` installed.
- Make sure you have access to the `create.sh` script and the necessary permissions to run it.
- 8 free GB of RAM

## Step 1: Create the VMs

To create the 5 VMs, use the `create.sh` script. This will set up the virtual machines required for the deployment.

Run the following command to create the VMs:

```bash
./create.sh
```

### Warning

It could be necessary to add this line in the script (or adapt to your config) in the virt-install:

```bash
        --network network=default,model=virtio \
```

And run the following command (or adapt to your config) in a terminal:

```bash
echo "net.ipv4.ip_forward = 1" | sudo tee -a /etc/sysctl.conf
sudo iptables -A FORWARD -i virbr0 -o wlp0s20f3 -j ACCEPT
sudo iptables -A FORWARD -i wlp0s20f3 -o virbr0 -m state --state RELATED,ESTABLISHED -j ACCEPT
```

## Step 2: Configure Ansible Vault

Before deploying with Ansible, you need to set up the Ansible Vault password.

Set vault password:
```bash
export ANSIBLE_VAULT_PASSWORD_FILE=/tmp/.vault_pass
echo xxx > /tmp/.vault_pass
```

**Note**: Replace xxx with your actual vault password.

## Step 3: Deploy with Ansible

To deploy the configuration, run the following command:

```bash
ansible-playbook -i inventory.ini playbook.yml
```
