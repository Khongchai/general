function* gen() {
	let index = 0;

	while (true){
		let value = yield index;
		if (value === "+")  index++;
		else index--;
	}
}

const g = gen();

//first time throwaway call
g.next();

console.log(g.next("+").value); // 1
console.log(g.next("-").value); // 0
console.log(g.next("+").value); // 1
console.log(g.next("+").value); // 2
console.log(g.next("+").value); // 3


