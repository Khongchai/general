interface Animal {
  name: string;
}

class Dog implements Animal {
  name: string;
}

class Cat implements Animal {
  name: string;
}

class AnimalShelter<T> {
  animals: T[] = [];

  adopt(animal: T) {
    this.animals.push(animal);
  }
}

const shelter = new AnimalShelter<Animal>();

shelter.adopt(new Dog());
shelter.adopt(new Cat());
