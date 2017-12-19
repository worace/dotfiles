require 'irb/completion'
require 'pp'
IRB.conf[:AUTO_INDENT] = true

require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

class Method; def open_in_vim; `mvim #{__file__}`; end; end

if File.exists?('script/console') && File.exists?('config/boot.rb')
  unless Object.const_defined?("RAILS_ROOT")
    require 'config/boot' unless Object.const_defined?("RAILS_ROOT")
    require 'commands/console' if ENV['RAILS_ENV'].nil?
    require File.join(RAILS_ROOT, 'config', 'environment')
  end
  puts "Rails #{Rails.version} #{RAILS_ENV} environment loaded."
end

def load_irbrc(path)
  return if (path == ENV["HOME"]) || (path == '/')

  load_irbrc(File.dirname path)

  irbrc = File.join(path, ".irbrc")

  load irbrc if File.exists?(irbrc)
end

load_irbrc Dir.pwd # probably should stay at the bottom
