interface Instrument {
    use(): void;
}

class Computer implements Instrument {
    async start(): Promise<void> {
        throw new Error("Method not implemented.");
    }

    shutdown(): void {
        throw new Error("Method not implemented.");
    }

    private compute(): void {

    }

    async apply(): Promise<void> {
        await this.start();
        this.use();
        await this.shutdown();
    }

    use(): void {
        this.compute();
    }

}