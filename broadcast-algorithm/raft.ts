// https://www.youtube.com/watch?v=uXEYuDwm7e4&list=PLeKd45zvjcDFUEv_ohr_HdUFe97RItdiB&index=18

let nodeIdCounter = 0;

type Role = "follower" | "candidate" | "leader";

type Message = string;

type Log = {
  message: Message;
  term: number;
};

type VoteRequest = {
  nodeId: NodeInstance["nodeId"];
  currentTerm: number;
  logLength: number;
  lastTerm: number;
};

const ELECTION_TIMEOUT_MS = 300;

// all methods async to simulate network.
class NodeInstance {
  currentTerm = 0;
  nodeId = nodeIdCounter++;
  votedFor: NodeInstance["nodeId"] | null = null;
  currentRole: Role = "follower";
  votesReceived = new Set<NodeInstance["nodeId"]>();
  sentLength: Record<NodeInstance["nodeId"], number> = {};
  ackedLength: Record<NodeInstance["nodeId"], number> = {};
  currentLeader: NodeInstance["nodeId"] | null = null;
  lastTerm = 0;
  logs: Log[] = [];
  commitLength = 0;
  cluster: NodeInstance[] = [];
  electionTimeoutId: NodeJS.Timeout | null = null;

  constructor(cluster: NodeInstance[]) {
    this.cluster = cluster;
  }

  // called when recover from crash or when election times out.
  async resetState() {
    this.currentRole = "follower";
    this.currentLeader = null;
    this.votesReceived = new Set();
    this.sentLength = {};
    this.ackedLength = {};
  }

  // when leader has failed or when election times out.
  async nextTerm() {
    this.currentTerm++;
    this.currentRole = "candidate";
    this.votedFor = this.nodeId;
    this.votesReceived.add(this.nodeId);
    // grabs the last term from the log
    // this is separate from _currentTerm because lastTerm keeps track of last commited log term.
    // a term might be empty so the two numbers can drift apart.
    this.lastTerm = this.logs.at(-1)?.term ?? 0;

    const message: VoteRequest = {
      nodeId: this.nodeId,
      currentTerm: this.currentTerm,
      logLength: this.logs.length,
      lastTerm: this.lastTerm,
    };

    const res = await Promise.race([
      await Promise.all(
        this.cluster.map((p) => {
          if (p.nodeId === this.nodeId) return;
          return p.onRequestToVote(message);
        }),
      ),
      new Promise<"timeout">((resolve) => {
        this.electionTimeoutId = setTimeout(() => {
          resolve("timeout");
          if (this.electionTimeoutId) {
            clearTimeout(this.electionTimeoutId);
            this.electionTimeoutId = null;
          }
        }, ELECTION_TIMEOUT_MS);
      }),
    ]);
    if (res === "timeout") {
      this.resetState();
      if (this.electionTimeoutId) {
        clearTimeout(this.electionTimeoutId);
        this.electionTimeoutId = null;
      }
      return;
    }

    // TODO handle
  }

  // called by a candidate.
  async onRequestToVote(message: VoteRequest) {
    const weAreBehind = message.currentTerm > this.currentTerm;
    if (weAreBehind) {
      this.currentTerm = message.currentTerm;
      this.currentRole = "follower";
      this.votedFor = null;
    }
  }
}
