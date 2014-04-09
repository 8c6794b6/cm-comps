// Alias of 'alert' function.
function foo(str)
{
  alert(str);
}

// Function taking two integer and returns the sum.
function bar(x,y)
{
  return (x+y);
}

// For reading file.
function getFileName(elem)
{
  var files = elem.files;
  var retval = "";
  for (var i=0, f; f=files[i]; i++) {
    retval = f.name;
  }
  return retval;
}
