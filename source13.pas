program Source13

  var
    x: Integer;
    y: Integer;
    result: Boolean

  begin

    read x;
    read y;
    if x > y // then expected
        result := true
    else
        result := false;
    write result

  end.
