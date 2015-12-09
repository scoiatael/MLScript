#
# Cookbook Name:: ubuntu-haskell-llvm
# Recipe:: default

include_recipe 'apt'

PACKAGES =
  %w(
    clang-3.5
    cabal-install
    libncurses-dev
    libzip-dev
    libedit-dev
    g++
    stack
  )

apt_repository 'zenoss' do
  uri 'http://download.fpcomplete.com/ubuntu'
  components ['main']
  distribution 'trusty'
  key '575159689BEFB442'
  keyserver 'keyserver.ubuntu.com'
  action :add
end

PACKAGES.each do |pkg|
  package pkg do
    action :install
  end
end
