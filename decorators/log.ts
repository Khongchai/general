function willLog(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const original = descriptor.value;
     descriptor.value = function(...args: any) {
      console.log(`Arguments: ${args}`);
      try {
        const result = original(...args);
        // or const result = original.apply(this, args); 
        // we use null because "this" can actually be null, 
        // and according to the MDN docs, we can just use the spread syntax

        
        console.log(`Result: ${result}`);
        return result;
      } catch (e) {
        console.log(`Error: ${e}`);
        throw e;
      }
    }
    
    return descriptor;
  };

class MathCollection{

    @willLog
    static sum(a: number, b: number){
        return a + b;
    }
}

MathCollection.sum(100, 100);

// modified from https://www.sitepoint.com/javascript-decorators-what-they-are/:wq
