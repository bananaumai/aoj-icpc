def solve(auns)
  checker = 0   
  auns.each do |aun|
    if aun == "A" then
      checker += 1
    else
      checker -= 1
    end
    return "NO" if checker < 0
  end
  if checker == 0 then
    "YES"
  else
    "NO"
  end
end

n = STDIN.gets.chomp.to_i
auns = []
n.times { auns.push(STDIN.gets.chomp) }
puts solve auns