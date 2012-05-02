
t0 = Time.new
# p(t0.utc?)

t1 = Time.new(1500000000.0e6)
p(t1.asctime)
p(t1.usec)
p(t1.to_i)
p(t1.to_f)
p(t1.zone)

p(t1.wday)
p(t1.yday)
p(t1.year)


t2 = Time.new(7.0e6)
t1.initialize_copy(t2)
p(t2.utc?)
p(t2.usec())


