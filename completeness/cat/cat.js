fs = require('fs')
file = process.argv[1]
data = fs.readFileSync(file, 'utf8')
console.log(data)
