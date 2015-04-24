importScripts("lib/spark-md5.min.js");

onmessage = function (e) {

  var file = e.data;

  // incremential hash settings
  var chunkSize = 2 * 1024 * 1024,  // 2MB chunks
      currentChunk = 0,
      chunks = Math.ceil(file.size / chunkSize),
      spark = new SparkMD5.ArrayBuffer(),
      reader = new FileReader();

  // load next chunk
  var loadNext = function () {
    var start = currentChunk * chunkSize,
        end   = ((start + chunkSize) >= file.size) ? file.size : start + chunkSize;
    reader.readAsArrayBuffer(file.slice(start, end));
  }

  reader.onload = function (e) {

    // hash current chunk
    spark.append(e.target.result);
    currentChunk++;

    if (currentChunk < chunks) {
      postMessage({
        done: false,
        percent: 100 * (currentChunk * chunkSize) / file.size,
      });
      loadNext();
    }
    else {
      var md5 = spark.end();
      postMessage({
        done: true,
        result: md5,
      });
      close();
    }
  };

  loadNext();
}
