# frozen_string_literal: true

require 'csv'

if ARGV.empty?
  puts 'You need to provide report directory path!'
  exit 1
end

report_directory = ARGV[0]
unless Dir.exist? report_directory
  puts "Report directory [#{report_directory}] does not exist."
  exit 1
end

results = Hash.new { |hash, key| hash[key] = {} }
Dir.glob("#{report_directory}/*.report").each do |file|
  name = File.basename(file).split(/_bench.report/).first
  results[name][:benchmark_name] = name
  results[name][:total_time] = File.read(file).scan(/Total:\s*((?:\d|\.)+)/)[0][0].to_f
  results[name][:ok_responses] = begin
                                   File.read(file).scan(/\[OK\]\s*(\d+)/)[0][0].to_f
                                 rescue StandardError
                                   0
                                 end
  results[name][:avg_resp_time] = File.read(file).scan(/\s*Average:\s*(.*\w)/)[0][0]
  results[name][:req_per_s] = results[name][:ok_responses] / results[name][:total_time]
end

Dir.glob("#{report_directory}/*.stats").each do |file|
  name = File.basename(file).split(/_bench.stats/).first
  stats = File
          .read(file)
          .scan(/([0-9\.]+)%\s+([0-9\.]+)(\w+)/)[0..-2] # ignore the last sample, not very reliable
          .map { |cpu, memory, mem_unit| [cpu.to_f, memory.to_f * (mem_unit == 'GiB' ? 1024 : 1)] }
          .reject { |cpu, _mem| cpu < 1 }
          .transpose

  results[name][:avg_mem_unit] = 'MiB'

  if stats[0].nil?
    puts "Warning: no stats for #{file}"
    results[name][:avg_cpu] = 0
    results[name][:avg_mem] = 0
  else
    results[name][:avg_cpu] = stats[0].sum / stats[0].length
    results[name][:avg_mem] = stats[1].sum / stats[1].length
  end
end

make_horizontal_line = -> { puts '-' * 76 }
make_data_line = lambda do |*args|
  puts "| #{args[0].to_s.ljust(18)} |" \
       "#{args[1].to_s.rjust(8)} |" \
       "#{args[2].to_s.rjust(15)} |" \
       "#{args[3].to_s.rjust(9)} |" \
       "#{args[4].to_s.rjust(14)} |"
end
make_horizontal_line[]
make_data_line['name', 'req/s', 'avg. latency', 'avg. cpu', 'avg. memory']
make_horizontal_line[]
results.sort_by { |_k, v| v[:req_per_s] }.reverse_each do |name, result|
  make_data_line[name,
                 result[:req_per_s].round(0),
                 result[:avg_resp_time],
                 result[:avg_cpu].round(2).to_s + '%',
                 result[:avg_mem].round(2).to_s + ' ' + result[:avg_mem_unit]]
end
make_horizontal_line[]

CSV.open("#{report_directory}/report.csv", 'w') do |csv|
  csv << results.first[1].keys
  results.values.each { |val| csv << val.values }
end
