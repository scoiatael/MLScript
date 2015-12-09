# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = 'ubuntu/trusty64'
  config.vm.synced_folder 'mlscript', '/opt/mlscript'

  config.vm.provider 'virtualbox' do |vb|
    #   # Display the VirtualBox GUI when booting the machine
    #   vb.gui = true

    # Customize the amount of memory on the VM:
    vb.memory = '2048'
  end

  # Enable provisioning with chef-solo. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  config.vm.provision 'chef_solo' do |chef|
    chef.add_recipe 'ubuntu-haskell-llvm'
  end
end
