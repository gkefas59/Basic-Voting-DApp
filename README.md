# 🗳️ Basic Voting DApp

A transparent and immutable on-chain voting system built on the Stacks blockchain using Clarity smart contracts. Vote once per proposal with complete transparency and tamper-proof results! 

## ✨ Features

- 📝 **Create Proposals**: Anyone can create voting proposals with custom options
- 🗳️ **One Vote Per User**: Each user can vote only once per proposal
- ⏰ **Time-based Voting**: Proposals have configurable duration periods
- 🔍 **Transparent Results**: All votes and results are publicly verifiable
- 📊 **Real-time Tracking**: Monitor vote counts and proposal status
- 🏁 **Proposal Finalization**: Mark completed proposals as finalized
- ⏱️ **Extend Voting**: Proposal creators can extend voting duration
- 📈 **Voting History**: Track user participation across proposals

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- [Stacks Wallet](https://wallet.hiro.so/) for mainnet/testnet interactions

### Installation

1. Clone the repository:
```bash
git clone https://github.com/gkefas59/Basic-Voting-DApp.git
cd Basic-Voting-DApp
```

2. Check the contract:
```bash
clarinet check
```

3. Run tests:
```bash
clarinet test
```

## 📋 Contract Functions

### Public Functions

#### `create-proposal`
Create a new voting proposal.

**Parameters:**
- `title` (string-ascii 100): Proposal title
- `description` (string-ascii 500): Detailed description  
- `option-a` (string-ascii 50): First voting option
- `option-b` (string-ascii 50): Second voting option
- `duration` (uint): Voting period in blocks

**Example:**
```clarity
(contract-call? .Basic-Voting-DApp create-proposal 
  "Community Fund Allocation" 
  "Should we allocate 50% of treasury to development?" 
  "Yes" 
  "No" 
  u1000)
```

#### `vote`
Cast a vote on an active proposal.

**Parameters:**
- `proposal-id` (uint): ID of the proposal
- `option` (string-ascii 1): Vote choice ("A" or "B")

**Example:**
```clarity
(contract-call? .Basic-Voting-DApp vote u1 "A")
```

#### `finalize-proposal`
Mark a completed proposal as finalized.

**Parameters:**
- `proposal-id` (uint): ID of the proposal to finalize

#### `extend-voting`
Extend voting duration (only by proposal creator).

**Parameters:**
- `proposal-id` (uint): ID of the proposal
- `additional-blocks` (uint): Number of blocks to extend

### Read-Only Functions

#### `get-proposal`
Get detailed proposal information.

#### `get-proposal-results`
Get voting results for a proposal.

#### `get-active-proposals`
List all currently active proposals.

#### `get-finished-proposals`
List all completed proposals.

#### `has-voted`
Check if a user has voted on a specific proposal.

#### `get-voter-history`
Get list of proposals a user has participated in.

## 🎯 Usage Examples

### Creating a Proposal

```clarity
;; Create a proposal that lasts 1000 blocks (~7 days)
(contract-call? .Basic-Voting-DApp create-proposal 
  "Upgrade Protocol" 
  "Should we implement the new consensus mechanism?" 
  "Approve" 
  "Reject" 
  u1000)
```

### Voting on a Proposal

```clarity
;; Vote "A" (first option) on proposal #1
(contract-call? .Basic-Voting-DApp vote u1 "A")

;; Vote "B" (second option) on proposal #2  
(contract-call? .Basic-Voting-DApp vote u2 "B")
```

### Checking Results

```clarity
;; Get proposal details
(contract-call? .Basic-Voting-DApp get-proposal u1)

;; Get voting results
(contract-call? .Basic-Voting-DApp get-proposal-results u1)

;; Check if voting is still active
(contract-call? .Basic-Voting-DApp is-voting-active u1)
```

## 🏗️ Contract Architecture

### Data Structures

- **Proposals Map**: Stores all proposal data including votes and metadata
- **Votes Map**: Tracks individual user votes with timestamps  
- **Voter History**: Maintains participation history for each user

### Security Features

- ✅ One vote per user per proposal
- ✅ Time-based voting windows
- ✅ Creator-only proposal extensions
- ✅ Immutable vote records
- ✅ Transparent result calculation

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

Check contract syntax:

```bash
clarinet check
```

Interactive console:

```bash
clarinet console
```

## 📈 Roadmap

- [ ] 🎨 Web frontend interface
- [ ] 📱 Mobile-responsive design  
- [ ] 🔐 Multi-signature proposals
- [ ] 📊 Advanced analytics dashboard
- [ ] 🌐 Integration with Stacks naming system
- [ ] ⚖️ Weighted voting mechanisms

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙋‍♂️ Support

- 📧 Email: support@votingdapp.com
- 💬 Discord: [Join our community](https://discord.gg/votingdapp)
- 🐛 Issues: [GitHub Issues](https://github.com/gkefas59/Basic-Voting-DApp/issues)

---

Built with ❤️ on Stacks blockchain
