# Usage:
#  ruby myprogs/pleasance/word_count_second_column.rb < ffh.csv
#
# This does the same as
#  awk -F \t '{ print $2 }' ffh.csv | tr "," "\n" | sed -e 's/^ //' | sort | uniq -c | sort
# but with way fewer processes and bash-magic.
# All the bash stuff is really fun, but at the end of the day this stuff is important
# enough to me financially that I'd rather have code I can read and maintain quickly.

ds = {}
ARGF.each do |line|
  second = line.split("\t")[1]

  second.split(",").each do |x|
    x2 = x.strip
    if ds[x2]
      ds[x2] += 1
    else
      ds[x2] = 1
    end
  end
end

ds.sort_by { |_, v| v }.each { |k, v|
  puts "#{v}\t#{k}"
}
