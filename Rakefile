require 'rubygems'
require 'rake'

task :default do
  sh "mzc -z -d lib/ src/*.scm"
end

task :console do
  puts "Erlenmeyer Console - All Modules Loaded\n"
  exec "mzscheme -m -S ./lib/ -t lib/*.zo"
end

task :clean do
  sh "git clean -xf"
end