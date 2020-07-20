class TestResult
  attr_accessor :name, :total_time, :ok_responses, :req_per_second, :average_response_time

  def initialize(name, total_time, ok_responses, average_response_time)
    @name = name.split(/_test.report/).first
    @total_time = total_time
    @ok_responses = ok_responses
    @req_per_second = calculate_req_per_second(ok_responses, total_time)
    @average_response_time = average_response_time
  end

  private

  def calculate_req_per_second(ok_responses, total_time)
    (ok_responses / total_time).to_i
  end
end
