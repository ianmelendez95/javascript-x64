try {
  input = require('fs').readFileSync(0, 'utf-8')
  console.log(JSON.stringify(require('acorn').parse(input, {ecmaVersion: 2020})))
} catch (e) {
  console.error(JSON.stringify({ "type": "Error", "name": e.name, "message": e.message, ...e}))
}