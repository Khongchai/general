function useRef(value){
	return Object.seal({
		current: value
	});
}

const MAX_ITERATIONS_COUNT = 50;

function* useMultipleRefs(initialValue){
	let count = 0;

	while (count++ < MAX_ITERATIONS_COUNT){
		yield useRef(initialValue);
	}
}

// const useRefs = useMultipleRefs("initialValue");
// console.log(useRefs.next().value);
// console.log(useRefs.next().value);
// console.log(useRefs.next().value);
// 
// The above are the same as these below
const [mockRef1, mockRef2, mockRef3] = useMultipleRefs("initialValue");
console.log(mockRef1, mockRef2);


