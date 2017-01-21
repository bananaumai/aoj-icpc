def make_datasets(arr)
  res = []
  n = arr.shift
  while(n != 0 || arr.size > 1)
    res.push(arr.slice!(0, n))
    n = arr.shift
  end
  res
end

def make_score(dataset)
  ((dataset.sort.slice(1, dataset.size - 2).inject(0.0){|acc, n| acc + n}) / (dataset.size - 2)).floor
end

make_datasets($stdin.readlines.map(&:chomp).map(&:to_i)).map(&method(:make_score)).each {|n| puts n}
