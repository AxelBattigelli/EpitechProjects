#!/bin/sh

set -euo pipefail

VM_IMAGE_DIR="VM"
VM_DISK_SIZE="10G"
VM_RAM="1024"
VM_CPU="2"
VM_NET_BRIDGE="br0"
PART_URL="https://cloud.debian.org/images/cloud/bookworm/latest/debian-12-generic-amd64.qcow2"
PART_PATH="./PART/debian-12-generic-amd64.qcow2"

if [ ! -f "$PART_PATH" ]; then
  echo "Download Debian..."
  mkdir -p $(dirname $PART_PATH)
  wget -O "$PART_PATH" "$PART_URL"
else
  echo "PARTITION found : $PART_PATH"
fi

mkdir -p $VM_IMAGE_DIR

declare -A VM_GROUPS=(
    ["redis"]="redis-1"
    ["postgresql"]="postgresql-1"
    ["poll"]="poll-1"
    ["result"]="result-1"
    ["worker"]="worker-1"
)

sudo virsh net-start default || true

for group in "${!VM_GROUPS[@]}"; do
    VM_NAME="${VM_GROUPS[$group]}"
    VM_IMAGE="${VM_IMAGE_DIR}/${VM_NAME}.qcow2"

    sudo virsh -c "qemu:///system" shutdown $VM_NAME || true
    while sudo virsh domstate $VM_NAME >/dev/null 2>&1; do sleep 0.1; done
    rm -f $VM_IMAGE || true
    rm -f "${VM_IMAGE_DIR}/${VM_NAME}" || true
    sed "s/@VMNAME@/${VM_NAME}/" > "${VM_IMAGE_DIR}/${VM_NAME}" <meta-data
    cp $PART_PATH $VM_IMAGE
    
    sudo virt-install \
        --connect "qemu:///system" \
        --name $VM_NAME \
        --cpu host-model \
        --import \
        --disk $VM_IMAGE \
        --osinfo debian12 \
        --vcpus $VM_CPU \
        --transient \
        --cloud-init user-data=user-data,meta-data="${VM_IMAGE_DIR}/${VM_NAME}" \
        --network network=default,model=virtio \
        &

    echo "The VM $VM_NAME (group: $group) was created successfully."

done

wait
