require 'rubygems'
require 'rake'

task :default do
  sh "mzc -z -d lib/ src/*.scm"
end

task :clean do
  sh "git clean -xf"
end