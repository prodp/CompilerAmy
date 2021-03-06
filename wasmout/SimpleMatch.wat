(module 
  (import "system" "printInt" (func $Std_printInt (param i32) (result i32)))
  (import "system" "printString" (func $Std_printString (param i32) (result i32)))
  (import "system" "readString0" (func $js_readString0 (param i32) (result i32)))
  (import "system" "readInt" (func $Std_readInt (result i32)))
  (import "system" "mem" (memory 100))
  (global (mut i32) i32.const 0) 

  (func $String_concat (param i32 i32) (result i32) (local i32 i32)
    get_global 0
    set_local 3
    get_local 0
    set_local 2
    loop $label_1
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_1
      else
      end
    end
    get_local 1
    set_local 2
    loop $label_2
      get_local 2
      i32.load8_u
      if
        get_local 3
        get_local 2
        i32.load8_u
        i32.store8
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        get_local 2
        i32.const 1
        i32.add
        set_local 2
        br $label_2
      else
      end
    end
    loop $label_0
      get_local 3
      i32.const 0
      i32.store8
      get_local 3
      i32.const 4
      i32.rem_s
      if
        get_local 3
        i32.const 1
        i32.add
        set_local 3
        br $label_0
      else
      end
    end
    get_global 0
    get_local 3
    i32.const 1
    i32.add
    set_global 0
  )

  (func $Std_digitToString (param i32) (result i32) 
    get_global 0
    get_local 0
    i32.const 48
    i32.add
    i32.store
    get_global 0
    get_global 0
    i32.const 4
    i32.add
    set_global 0
  )

  (func $Std_readString (result i32) 
    get_global 0
    get_global 0
    call $js_readString0
    set_global 0
  )

  (func $Std_printBoolean (param i32) (result i32) 
    get_local 0
    call $Std_booleanToString
    call $Std_printString
  )

  (func $Std_intToString (param i32) (result i32) (local i32 i32)
    get_local 0
    i32.const 0
    i32.lt_s
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 45
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 4
      i32.add
      set_global 0
      get_local 0
      i32.const -1
      i32.mul
      call $Std_intToString
      call $String_concat
    else
      get_local 0
      i32.const 10
      i32.rem_s
      set_local 1
      get_local 0
      i32.const 10
      i32.div_s
      set_local 2
      get_local 2
      i32.const 0
      i32.eq
      if (result i32)
        get_local 1
        call $Std_digitToString
      else
        get_local 2
        call $Std_intToString
        get_local 1
        call $Std_digitToString
        call $String_concat
      end
    end
  )

  (func $Std_booleanToString (param i32) (result i32) 
    get_local 0
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 116
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 114
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 117
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    else
      get_global 0
      i32.const 0
      i32.add
      i32.const 102
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 97
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 108
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 115
      i32.store8
      get_global 0
      i32.const 4
      i32.add
      i32.const 101
      i32.store8
      get_global 0
      i32.const 5
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 6
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      i32.const 7
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 8
      i32.add
      set_global 0
    end
  )
  (export "SimpleMatch_main" (func $SimpleMatch_main))
  (func $SimpleMatch_main (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    get_global 0
    set_local 1
    get_global 0
    i32.const 12
    i32.add
    set_global 0
    get_local 1
    i32.const 1
    i32.store
    get_local 1
    i32.const 4
    i32.add
    i32.const 5
    i32.store
    get_local 1
    i32.const 8
    i32.add
    get_global 0
    set_local 2
    get_global 0
    i32.const 4
    i32.add
    set_global 0
    get_local 2
    i32.const 0
    i32.store
    get_local 2
    i32.store
    get_local 1
    set_local 0
    get_local 0
    set_local 4
    get_local 4
    i32.load
    i32.const 0
    i32.eq
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end
    if (result i32)
      get_global 0
      i32.const 0
      i32.add
      i32.const 78
      i32.store8
      get_global 0
      i32.const 1
      i32.add
      i32.const 105
      i32.store8
      get_global 0
      i32.const 2
      i32.add
      i32.const 108
      i32.store8
      get_global 0
      i32.const 3
      i32.add
      i32.const 0
      i32.store8
      get_global 0
      get_global 0
      i32.const 4
      i32.add
      set_global 0
    else
      get_local 4
      i32.load
      i32.const 1
      i32.eq
      if (result i32)
        get_local 4
        i32.const 4
        i32.add
        i32.load
        set_local 5
        get_local 5
        i32.const 5
        i32.eq
        if (result i32)
          i32.const 1
        else
          i32.const 0
        end
        get_local 4
        i32.const 8
        i32.add
        i32.load
        set_local 6
        get_local 6
        i32.load
        i32.const 0
        i32.eq
        if (result i32)
          i32.const 1
        else
          i32.const 0
        end
        if (result i32)
          i32.const 1
        else
          i32.const 0
        end
        i32.and
      else
        i32.const 0
      end
      if (result i32)
        get_global 0
        i32.const 0
        i32.add
        i32.const 79
        i32.store8
        get_global 0
        i32.const 1
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 2
        i32.add
        i32.const 101
        i32.store8
        get_global 0
        i32.const 3
        i32.add
        i32.const 32
        i32.store8
        get_global 0
        i32.const 4
        i32.add
        i32.const 67
        i32.store8
        get_global 0
        i32.const 5
        i32.add
        i32.const 111
        i32.store8
        get_global 0
        i32.const 6
        i32.add
        i32.const 110
        i32.store8
        get_global 0
        i32.const 7
        i32.add
        i32.const 115
        i32.store8
        get_global 0
        i32.const 8
        i32.add
        i32.const 33
        i32.store8
        get_global 0
        i32.const 9
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 10
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        i32.const 11
        i32.add
        i32.const 0
        i32.store8
        get_global 0
        get_global 0
        i32.const 12
        i32.add
        set_global 0
      else
        get_local 4
        i32.load
        i32.const 1
        i32.eq
        if (result i32)
          get_local 4
          i32.const 4
          i32.add
          i32.load
          set_local 7
          get_local 7
          i32.const 5
          i32.eq
          if (result i32)
            i32.const 1
          else
            i32.const 0
          end
          get_local 4
          i32.const 8
          i32.add
          i32.load
          set_local 8
          get_local 8
          i32.load
          i32.const 1
          i32.eq
          if (result i32)
            get_local 8
            i32.const 4
            i32.add
            i32.load
            set_local 9
            get_local 9
            i32.const 5
            i32.eq
            if (result i32)
              i32.const 1
            else
              i32.const 0
            end
            get_local 8
            i32.const 8
            i32.add
            i32.load
            set_local 10
            get_local 10
            i32.load
            i32.const 0
            i32.eq
            if (result i32)
              i32.const 1
            else
              i32.const 0
            end
            if (result i32)
              i32.const 1
            else
              i32.const 0
            end
            i32.and
          else
            i32.const 0
          end
          if (result i32)
            i32.const 1
          else
            i32.const 0
          end
          i32.and
        else
          i32.const 0
        end
        if (result i32)
          get_global 0
          i32.const 0
          i32.add
          i32.const 84
          i32.store8
          get_global 0
          i32.const 1
          i32.add
          i32.const 119
          i32.store8
          get_global 0
          i32.const 2
          i32.add
          i32.const 111
          i32.store8
          get_global 0
          i32.const 3
          i32.add
          i32.const 32
          i32.store8
          get_global 0
          i32.const 4
          i32.add
          i32.const 67
          i32.store8
          get_global 0
          i32.const 5
          i32.add
          i32.const 111
          i32.store8
          get_global 0
          i32.const 6
          i32.add
          i32.const 110
          i32.store8
          get_global 0
          i32.const 7
          i32.add
          i32.const 115
          i32.store8
          get_global 0
          i32.const 8
          i32.add
          i32.const 33
          i32.store8
          get_global 0
          i32.const 9
          i32.add
          i32.const 33
          i32.store8
          get_global 0
          i32.const 10
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          i32.const 11
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          get_global 0
          i32.const 12
          i32.add
          set_global 0
        else
          get_global 0
          i32.const 0
          i32.add
          i32.const 77
          i32.store8
          get_global 0
          i32.const 1
          i32.add
          i32.const 97
          i32.store8
          get_global 0
          i32.const 2
          i32.add
          i32.const 116
          i32.store8
          get_global 0
          i32.const 3
          i32.add
          i32.const 99
          i32.store8
          get_global 0
          i32.const 4
          i32.add
          i32.const 104
          i32.store8
          get_global 0
          i32.const 5
          i32.add
          i32.const 32
          i32.store8
          get_global 0
          i32.const 6
          i32.add
          i32.const 101
          i32.store8
          get_global 0
          i32.const 7
          i32.add
          i32.const 114
          i32.store8
          get_global 0
          i32.const 8
          i32.add
          i32.const 114
          i32.store8
          get_global 0
          i32.const 9
          i32.add
          i32.const 111
          i32.store8
          get_global 0
          i32.const 10
          i32.add
          i32.const 114
          i32.store8
          get_global 0
          i32.const 11
          i32.add
          i32.const 33
          i32.store8
          get_global 0
          i32.const 12
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          i32.const 13
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          i32.const 14
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          i32.const 15
          i32.add
          i32.const 0
          i32.store8
          get_global 0
          get_global 0
          i32.const 16
          i32.add
          set_global 0
          call $Std_printString
          unreachable
        end
      end
    end
    set_local 3
    get_local 3
    call $Std_printString
    drop
  )
)