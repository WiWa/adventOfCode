var util = require('util');
var fs = require('fs');

function o2str(o) {
  return util.inspect(o, false, null);
}

function forEach(arr, f){
  for(var i = 0; i < arr.length; i++){
    f(i, arr[i]);
  }
}

function readAndCall(file, f){
  fs.readFile(file, 'utf8', function(err,data){
    if(err) {
      return console.log(err);
    }
    else {
      f(data);
    }
  });
}

function getDim(str){
  var xyz = str.split("x");
  forEach(xyz, function(i,v){ xyz[i] = parseInt(v); });
  return { x: xyz[0], y: xyz[1], z: xyz[2] }
}

function getAreaPlus(dim){
  var lw = dim.x * dim.y;
  var wh = dim.y * dim.z;
  var lh = dim.x * dim.z;
  return 2 * (lw + wh + lh) + Math.min(lw, wh, lh);
}

function vol(dim){
  return dim.x * dim.y * dim.z;
}

function getMinN(arr, n){
  var tmp = [];
  forEach(arr, function(i, v){
    tmp.push(v);
  })
  var ans = [];
  var min;
  while(n > 0){
    min = Math.min.apply(this,tmp);
    ans.push(min);
    tmp.splice(tmp.indexOf(min), 1);
    n--;
  }
  return ans;
}

function dArr(dim){
  return [dim.x, dim.y, dim.z];
}

function readDims(str){
  var dims = [];
  var strs = str.split("\n");
  forEach(strs, function(i,v){
    if(v.trim() == ""){
      return true;
    }
    dims.push(getDim(v));
  });
  return dims
}

function getTotalDim(str){
  var sum = 0;
  var dims = readDims(str);

  forEach(dims, function(i,dim){
    sum += getAreaPlus(dim);
  });

  return sum;
}

function getRibbonLength(str){
  var sum = 0;
  var dims = readDims(str);
  
  forEach(dims, function(i,dim){
    var min2 = getMinN(dArr(dim), 2);
    sum += (2 * min2[0]) + (2 * min2[1]);
    sum += vol(dim);
  });

  return sum;
}

function doLog(fArr){
  return function(data){
    forEach(fArr, function(i, f){
      console.log(f(data));
    })
  }
}

function main(){

  var file = process.argv[2];
  if(file){
    readAndCall(file,
      doLog([
        getTotalDim,
        getRibbonLength
      ]));
  }

}

main();
