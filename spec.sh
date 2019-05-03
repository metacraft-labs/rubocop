cd ~/rubocop
# rbenv local 2.5.0

# rbenv exec bundle exec ruby /home/alehander42/.rbenv/versions/2.5.0/bin/rspec ~/rubocop/spec/rubocop/cop/$2/$1_spec.rb
# cd ~/rubocop
bundle exec rspec ~/rubocop/spec/rubocop/cop/$2/$1_spec.rb

