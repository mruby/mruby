$iso_methods = [
  [], # 15.1

  [ # 15.2
    [:Object, [
        [], # description
        [], # superclass
        [], # included modules
        [], # constants
        [:initialize], # instance methods
      ]],

    [:Module, [
        [], # description
        [], # superclass
        [:constants, :nesting], # singleton methods
        [
          :<, :<=, :<=>, :==, :===, :>, :>=,
          :alias_method, :ancestors, :append_features,
          :attr, :attr_accessor, :attr_reader, :attr_writer,
          :class_eval, :class_variable_defined?, :class_variable_get, :class_variable_set,
          :class_variables, :const_defined?, :const_get, :const_missing, :const_set,
          :constants, :extend_object, :extended, :include, :included?, :included_modules,
          :method_defined?, :module_eval, :private, :protected, :public,
          :remove_class_variable, :remove_const, :remove_method, :undef_method,
        ], # instance methods
      ]],

    [:Class, [
        [], # description
        [], # superclass
        [:initialize, :initialize_copy, :new, :superclass] # instance methods
      ]],

    [:NilClass, [
        [], # description
        [], # superclass
        [:&, :~, :|, :nil?, :to_s] # instance methods
      ]],

    [:TrueClass, [
        [], # description
        [], # superclass
        [:&, :~, :to_s, :|], # instance methods
      ]],

    [:FalseClass, [
        [], # description
        [], # superclass
        [:&, :~, :to_s, :|], # instance methods
      ]],

    [:Numeric, [
        [],
        [],
        [],
        [:+@, :-@, :abs, :coerce],
      ]],

    [:Integer, [
        [],
        [],
        [:+, :-, :*, :/, :%, :<=>, :==, :~, :&, :|, :^, :<<, :>>,
          :ceil, :downto, :eql?, :floor, :hash, :next, :round, :succ, :times,
          :to_f, :to_i, :to_s, :truncate, :upto],
      ]],

    [:Float, [
        [],
        [],
        [:+, :-, :*, :/, :%, :<=>, :==, :ceil, :finite?, :floor, :infinite?, :round,
          :to_f, :to_i, :truncate],
      ]],

    [:String, [
        [],
        [],
        [],
        [],
        [:*, :+, :<=>, :==, :=~, :[], :capitalize, :capitalize!, :chomp, :chomp!, :chop, :chop!,
          :downcase, :downcase!, :each_line, :empty?, :eql?, :gsub, :gsub!, :hash,
          :include?, :index, :initialize, :initialize_copy, :intern, :length,
          :match, :replace, :reverse, :reverse!, :rindex, :scan, :size, :slice,
          :split, :sub, :sub!, :to_i, :to_f, :to_s, :to_sym, :upcase, :upcase!],
      ]],

    [:Symbol, [
        [],
        [],
        [:===, :id2name, :to_s, :to_sym]
      ]],

    [:Array, [
        [],
        [],
        [],
        [:[]],
        [:*, :+, :<<, :[], :[]=, :clear, :collect!, :concat, :delete_at, :each, :each_index,
          :empty?, :first, :index, :initialize, :initialize_copy, :join, :last,
          :length, :map!, :pop, :push, :replace, :reverse, :reverse!, :rindex, :shift,
          :size, :slice, :unshift]
      ]],

    [:Hash, [
        [],
        [],
        [],
        [:==, :[], :[]=, :clear, :default, :default=, :default_proc, :delete, :each,
          :each_key, :each_value, :empty?, :has_key?, :has_value?, :include?,
          :initialize, :initialize_copy, :key?, :keys, :length, :member?,
          :merge, :replace, :shift, :size, :store, :value?, :values],
      ]],

    [:Range, [
        [],
        [],
        [],
        [:==, :===, :begin, :each, :end, :exclude_end?, :first, :include?, :initialize,
          :last, :member?],
      ]],

    [:Regexp, [
        [],
        [],
        [],
        [],
        [],
        [:compile, :escape, :last_match, :quote],
        [:initialize, :initialize_copy, :==, :===, :=~, :casefold?, :match, :source]
      ]],

    [:MatchData, [
        [],
        [],
        [:[], :begin, :captures, :end, :initialize_copy, :length, :offset, :post_match,
          :pre_match, :size, :string, :to_a, :to_s],
      ]],

    [:Proc, [
        [],
        [],
        [:new],
        [:[], :arity, :call, :clone, :dup],
      ]],

    [:Struct, [
        [],
        [],
        [:new],
        [:==, :[], :[]=, :each, :each_pair, :members, :select,
          :initialize, :initialize_copy],
      ]],

    [:Time, [
        [],
        [],
        [],
        [],
        [],
        [:at, :gm, :local, :mktime, :now, :utc],
        [:+, :-, :<=>, :asctime, :ctime, :day, :dst?, :getgm, :getlocal, :getutc,
          :gmt?, :gmt_offset, :gmtime, :gmtoff, :hour, :initialize, :initialize_copy,
          :localtime, :mday, :min, :mon, :month, :sec, :to_f, :to_i, :usec, :utc,
          :utc?, :utc_offset, :wday, :yday, :year, :zone]
      ]],

    [:IO, [
        [],
        [],
        [],
        [:open],
        [:close, :closed?, :each, :each_byte, :each_line, :eof?, :flush, :getc, :gets,
          :initialize_copy, :print, :putc, :puts, :read, :reachar, :readline, :readlines,
          :sync, :sync=, :write]
      ]],

    [:File, [
        [],
        [],
        [:exist?],
        [:initialize, :path],
      ]],

    [:Exception, [
        [],
        [],
        [],
        [:exception],
        [:exception, :message, :to_s, :initialize]
      ]],

    [:StandardError, [
        [],
        [],
      ]],

    [:ArgumentError, [
        [],
        [],
      ]],

    [:LocalJumpError, [
        [],
        [:exit_value, :reason],
      ]],

    [:RangeError, [
        [],
        [],
      ]],

    [:RegexpError, [
        [],
        [],
      ]],

    [:RuntimeError, [
        [],
        [],
      ]],

    [:TypeError, [
        [],
        [],
      ]],

    [:NameError, [
        [],
        [:name, :initialize],
      ]],

    [:NoMethodError, [
        [],
        [:name, :initialize],
      ]],

    [:IndexError, [
        [],
        [],
      ]],

    [:IOError, [
        [],
        [],
      ]],

    [:EOFError, [
        [],
        [],
      ]],

    [:SystemCallError, [
        [],
        [],
      ]],

    [:ScriptError, [
        [],
        [],
      ]],

    [:SyntaxError, [
        [],
        [],
      ]],

    [:LoadError, [
        [],
        [],
      ]],
  ],

  [ # 15.3
    [:Kernel, [
        [],
        [:`, :block_given?, :eval, :global_variables, :iterator?, :lambda, #`
          :local_variables, :loop, :p, :print, :puts, :raise, :require],
        [:==, :===, :__id__, :__send__, :`, #`
          :block_given?, :class, :clone, :dup, :eql?, :equal?, :eval, :extend,
          :global_variables, :hash, :initialize_copy, :inspect, :instance_eval,
          :instance_of?, :instance_variable_defined?, :instance_variable_get,
          :instance_variable_set, :instance_variables, :is_a?, :iterator?,
          :kind_of?, :lambda, :local_variables, :loop, :method_missing,
          :methods, :nil?, :object_id, :p, :print, :private_methods,
          :protected_methods, :public_methods, :puts, :raise,
          :remove_instance_variable, :require, :respond_to?, :send,
          :singleton_methods, :to_s]
      ]],

    [:Enumerable, [
        [],
        [:all?, :any?, :collect, :detect, :each_with_index, :entries,
          :find, :find_all, :grep, :include?, :inject, :map, :min, :member?,
          :partition, :reject, :select, :sort, :to_a]
      ]],

    [:Comparable, [
        [],
        [:<, :<=, :==, :>, :>=, :between?]
      ]]
  ]
]

$iso_methods_result = []

def mark_iso_done(iso)
  idx = iso.split('.').map { |v| v.to_i }

  if idx.size == 5 and idx[0] == 15 and (idx[1] == 2 or idx[1] == 3)
    idx = idx.map { |v| v - 1 }
    $iso_methods_result[idx[1]] ||= []
    $iso_methods_result[idx[1]][idx[2]] ||= []
    $iso_methods_result[idx[1]][idx[2]][idx[3]] ||= []
    $iso_methods_result[idx[1]][idx[2]][idx[3]][idx[4]] = :done
  end
end

def report_iso
  done_count = 0
  iso_count = 0

  t_print "ISO methods that isn't tested:\n" if $mrbtest_verbose

  $iso_methods.each_index do |sect_idx|
    sect = $iso_methods[sect_idx]
    next unless sect
    sect.each_index do |mod_idx|
      mod = sect[mod_idx]
      mod[1].each_index do |meths_idx|
        meths = mod[1][meths_idx]
        meth_sep = meths_idx == mod[1].length - 1 ? '#' : '.'
        meths.each_index do |meth_idx|
          iso_count += 1
          begin
            if $iso_methods_result[sect_idx][mod_idx][meths_idx][meth_idx] == :done
              done_count += 1
              next
            end
          rescue NoMethodError
          end

          meth = "#{mod[0]}#{meth_sep}#{meths[meth_idx]}"

          t_print "  #{meth} (15.#{sect_idx + 1}.#{mod_idx + 1}.#{meths_idx + 1}.#{meth_idx + 1})\n" if $mrbtest_verbose
        end
      end
    end
  end

  t_print "ISO methods test coverage: #{done_count}/#{iso_count}\n"
end

$iso_stats = true
