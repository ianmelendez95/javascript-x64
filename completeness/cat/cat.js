fs = require('fs')
args = process.argv
file = args[2]
data = fs.readFileSync(file, 'utf8')
console.log(data)
