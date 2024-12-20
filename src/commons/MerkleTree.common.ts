import { HttpException, HttpStatus } from '@nestjs/common';
import { SimpleMerkleTree } from '@openzeppelin/merkle-tree';
import keccak256 from 'keccak256';

export class MerkleTree {
  public root: string;
  public proof: string[][];
  public list: string[];
  public tree: SimpleMerkleTree;

  constructor(input: string[]) {
    this.list = input;
  }

  generateTree() {
    try {
      this.tree = SimpleMerkleTree.of(this.list);
      this.root = this.tree.root;
      this.proof = this.list.map((leaf, index) => this.tree.getProof(index));
    } catch (error) {
      const statusCode = error?.response?.statusCode || HttpStatus.BAD_REQUEST;
      throw new HttpException(`generateTree: ${error.message}`, statusCode);
    }
  }
}
