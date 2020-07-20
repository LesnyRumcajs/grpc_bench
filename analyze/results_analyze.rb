this_dir = File.expand_path(File.dirname(__FILE__))
lib_dir = File.join(this_dir, 'lib')
$LOAD_PATH.unshift(lib_dir) unless $LOAD_PATH.include?(lib_dir)

require 'test_result'

if ARGV.empty?
  puts 'You need to provide report directory path!'
  exit 1
end

report_directory = ARGV[0]
unless Dir.exists? report_directory
  puts "Report directory [#{report_directory}] does not exist."
  exit 1
end

results = []
files = Dir.glob("#{report_directory}/*.report")
files.each do |file|
  total_time = File.read(file).scan(/Total:\s*((?:\d|\.)+)/)[0][0].to_f
  ok_responses = File.read(file).scan(/\[OK\]\s*(\d+)/)[0][0].to_f rescue 0
  average_response_time = File.read(file).scan(/\s*Average:\s*(.*\w)/)[0][0]
  results << TestResult.new(File.basename(file), total_time, ok_responses, average_response_time)
end

make_horizontal_line = -> { puts '-'*50 }
make_data_line = -> (x,y,z) { puts "| #{x.to_s.ljust(20)}|#{y.to_s.rjust(8)} |#{z.to_s.rjust(15)} |" }
make_horizontal_line[]
make_data_line['name', 'req/s', 'avg. latency']
make_horizontal_line[]
results.sort_by(&:req_per_second).reverse_each do |result|
    make_data_line[result.name, result.req_per_second, result.average_response_time]
end
make_horizontal_line[]
