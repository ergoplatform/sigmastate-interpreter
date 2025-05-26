declare module "sigmastate-js/main" {
    import {
        Amount,
        Box as FBox,
        EIP12UnsignedInput,
        NonMandatoryRegisters, SignedTransaction, TokenAmount,
        UnsignedTransaction
    } from "@fleet-sdk/common";

    type SigmaCompilerNamedConstantsMap = { [key: string]: Value };
    type HexString = string;
    type ByteArray = { u: Int8Array };

    export declare class Address {
        /** Serialize this address to bytes. */
        addressBytes(): ByteArray;

        /** Address type code used to differentiate between pay-to-public-key, pay-to-script,
         * pay-to-script-hash addresses.
         *
         * @see [[P2PKAddress]], [[P2SAddress]], [[P2SHAddress]]
         */
        addressTypePrefix(): number;

        /** @return true if this address from Ergo mainnet. */
        isMainnet(): boolean;

        /** @return true if this address has Pay-To-Public-Key type. */
        isP2PK(): boolean;

        /** @return underlying {@link P2PKAddress}.
         * @throws IllegalArgumentException if this instance is not P2PK address
         */
        asP2PK(): P2PKAddress;

        /** @return true if this address has Pay-To-Script type. */
        isP2S(): boolean;

        /** @return underlying {@link P2PKAddress}.
         * @throws IllegalArgumentException if this instance is not P2PK address
         */
        asP2S(): P2SAddress;

        /** @return true if this address has Pay-To-Script-Hash type. */
        isP2SH(): boolean;

        /** @return underlying {@link P2SHAddress}.
         * @throws IllegalArgumentException if this instance is not P2SH address
         */
        asP2SH(): P2SHAddress;

        /** Extracts a {@link SigmaProp} from this address of the underlying ErgoTree if of
         * specific form.
         * @see ErgoTree.toSigmaBooleanOpt()
         */
        toSigmaPropOpt(): SigmaProp | undefined;

        /** ErgoTree which corresponds to the address (depending on the address type).
         *
         * @see P2PKAddress, P2SAddress, P2SHAddress
         */
        toErgoTree(): ErgoTree;

        /** @return this addresses ErgoTree's proposition bytes. Use this to store this address
         *         on Box registers.
         */
        toPropositionBytes(): ByteArray;

        /** Converts the given {@link Address} to Base58 string. */
        toString(): string;
    }

    export declare class Address$ {
        /** Creates JS wrapper over given [[ErgoAddress]]. */
        static fromErgoAddress(ergoAddress: any /*org.ergoplatform.ErgoAddress*/): Address;

        /** Deserializes an ErgoTree instance from an address string.
         *
         * @param base58String a Base58 string representing the serialized ErgoTree
         */
        static fromString(base58String: string): Address;

        /** Creates an `Address` instance from an `ErgoTree` and a network prefix.
         *
         * @param ergoTree      The `ErgoTree` instance to be converted into an `Address`.
         * @param networkPrefix The network prefix indicating the network for which the address is valid.
         * @return An `Address` instance corresponding to the given `ErgoTree` and network prefix.
         */
        static fromErgoTree(ergoTree: ErgoTree, networkPrefix: number): Address;

        /**
         * Creates an `Address` from a `SigmaProp` and a network prefix.
         *
         * @param sigmaProp     The `SigmaProp` to be converted into an `Address`.
         * @param networkPrefix The network prefix indicating the network for which the address is valid.
         * @return An `Address` instance corresponding to the given `SigmaProp` and network prefix.
         */
        static fromSigmaProp(sigmaProp: SigmaProp, networkPrefix: number): Address;

        /** Creates address from given ergovalue containing an ErgoTree proposition bytes.
         * Use this to convert a box register containing an ErgoTree into its address.
         *
         * @param networkPrefix    mainnet or testnet network
         * @param propositionBytes ErgoTree proposition bytes
         */
        static fromPropositionBytes(networkPrefix: number, propositionBytes: ByteArray): Address
    }

    /** Implementation of pay-to-public-key {@link Address}. */
    export declare class P2PKAddress extends Address {
        /** Converts this address to the underlying ProveDlog sigma proposition wrapped in {@link SigmaProp}. */
        toSigmaProp(): SigmaProp;

        /** Extract the underlying {@link GroupElement} of this address. */
        getPublicKeyGE(): GroupElement;
    }

    /** Implementation of pay-to-script {@link Address}. */
    export declare class P2SAddress extends Address {
    }

    /** Implementation of pay-to-script-hash {@link Address}. */
    export declare class P2SHAddress extends Address {
    }

    export declare class ErgoTree {
        toHex(): HexString;

        bytes(): ByteArray;

        header(): number;

        version(): number;

        isConstantSegregation(): boolean;

        hasSize(): boolean;

        constants(): Value[];

        template(): ByteArray;

        templateHex(): HexString;

        toString(): string;
    }

    export declare class ErgoTree$ {
        static fromHex(value: HexString): ErgoTree;
    }

    export declare class GroupElement {
        toPointHex(): HexString;
    }

    export declare class GroupElement$ {
        static fromPointHex(value: HexString): GroupElement;
    }

    export declare class SigmaProp {
    }

    export declare class SigmaProp$ {
        static fromPointHex(value: HexString): SigmaProp;
    }

    export declare class AvlTree {
        digest: HexString;
        insertAllowed: Boolean;
        updateAllowed: Boolean;
        removeAllowed: Boolean;
        keyLength: number;
        valueLengthOpt: number | undefined;
    }

    export declare class Box {
        box: FBox<Amount, NonMandatoryRegisters>;
    }

    export declare class PreHeader {
        /** Block version, to be increased on every soft and hardfork. */
        version: number;
        /** Hex of id of parent block */
        parentId: HexString;
        /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
        timestamp: bigint;
        /** Current difficulty in a compressed view.
         * NOTE: actually it is unsigned integer */
        nBits: bigint;
        /** Block height */
        height: number;
        /** Miner public key (hex of EC Point). Should be used to collect block rewards. */
        minerPk: GroupElement;
        /** Hex of miner votes bytes for changing system parameters. */
        votes: HexString;
    }

    export declare class Header {
        /** Hex representation of ModifierId of this Header */
        id: HexString;
        /** Block version, to be increased on every soft and hardfork. */
        version: number;
        /** Hex representation of ModifierId of the parent block */
        parentId: HexString;
        /** Hex hash of ADProofs for transactions in a block */
        ADProofsRoot: HexString;
        /** AvlTree of a state after block application */
        stateRoot: AvlTree;
        /** Hex of root hash (for a Merkle tree) of transactions in a block. */
        transactionsRoot: HexString;
        /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
        timestamp: bigint;
        /** Current difficulty in a compressed view.
         * NOTE: actually it is unsigned Int */
        nBits: bigint;
        /** Block height */
        height: number;
        /** Hex of root hash of extension section */
        extensionRoot: HexString;

        /** Miner public key (hex of EC Point). Should be used to collect block rewards.
         * Part of Autolykos solution.
         */
        minerPk: GroupElement;

        /** One-time public key (hex of EC Point). Prevents revealing of miners secret. */
        powOnetimePk: GroupElement;

        /** Hex of nonce bytes */
        powNonce: HexString;

        /** Distance between pseudo-random number, corresponding to nonce `powNonce` and a secret,
         * corresponding to `minerPk`. The lower `powDistance` is, the harder it was to find this solution. */
        powDistance: bigint;

        /** Miner votes for changing system parameters. */
        votes: HexString;
    }

    export declare class BlockchainParameters {
        storageFeeFactor: number;
        minValuePerByte: number;
        maxBlockSize: number;
        tokenAccessCost: number;
        inputCost: number;
        dataInputCost: number;
        outputCost: number;
        maxBlockCost: number;
        softForkStartingHeight: number | undefined;
        softForkVotesCollected: number | undefined;
        blockVersion: number;
    }

    export declare class BlockchainStateContext {
        sigmaLastHeaders: Header[];
        previousStateDigest: HexString;
        sigmaPreHeader: PreHeader;
    }

    export declare class Type {
        name: string;

        toString(): string;
    }

    export declare class Type$ {
        static Byte: Type;
        static Short: Type;
        static Int: Type;
        static Long: Type;
        static BigInt: Type;
        static GroupElement: Type;
        static SigmaProp: Type;
        static Box: Type;
        static AvlTree: Type;
        static Context: Type;
        static Header: Type;
        static PreHeader: Type;
        static SigmaDslBuilder: Type;

        static pairType(left: Type, right: Type): Type;

        static collType(elemType: Type): Type;
    }

    export declare class Value<T = unknown> {
        data: T;
        tpe: Type;

        toHex(): HexString;
    }

    export declare class Value$ {
        static ofByte(value: number): Value<number>;

        static ofShort(value: number): Value<number>;

        static ofInt(value: number): Value<number>;

        static ofLong(value: bigint): Value<bigint>;

        static ofBigInt(value: bigint): Value<bigint>;

        static ofGroupElement(pointHex: string): Value<GroupElement>;

        static ofSigmaProp(pointHex: string): Value<SigmaProp>;

        static ofBox(box: FBox<Amount, NonMandatoryRegisters>): Value<Box>;

        static pairOf<R, L>(left: Value<R>, right: Value<L>): Value<[R, L]>;

        static collOf<T>(items: T[], elemType: Type): Value<T[]>;

        static fromHex<T>(hex: HexString): Value<T>;
    }

    export declare class SigmaCompiler {
        compile(
            namedConstants: SigmaCompilerNamedConstantsMap,
            segregateConstants: boolean,
            additionalHeaderFlags: number,
            ergoScript: string
        ): ErgoTree;
    }

    export declare class SigmaCompiler$ {
        static forMainnet(): SigmaCompiler;

        static forTestnet(): SigmaCompiler;
    }

    /** Represents results for transaction reduction by {@link SigmaProver}. */
    export declare class ReducedTransaction {
        /** Serialized bytes of this transaction in hex format. */
        toHex(): HexString;
    }

    export declare class ReducedTransaction$ {
        /** Creates a {@link ReducedTransaction} from serialized bytes in hex format. */
        fromHex(hex: HexString): ReducedTransaction;
    }

    /** Represents hints used by [[SigmaPropProver]] to perform operations as part of
     * multi-signature scheme. See [EIP-11](https://github.com/ergoplatform/eips/pull/8).
     */
    export declare class ProverHints {

    }

    export declare class ProverHints$ {
        /** Empty bag of hints. Immutable value can be reused where necessary. */
        empty(): ProverHints
    }

    /** Represents one secret (aka SigmaProtocolPrivateInput) used by [[SigmaPropProver]]. */
    export declare class ProverSecret {
        /** Public key generated from the secret.
         * Represents proof of knowledge sigma proposition.
         */
        publicKey(): SigmaProp

        /** Secret random number stored in this instance. */
        secret(): bigint
    }
    
    export declare class ProverSecret$ {
        /** Creates a new [[ProverSecret]] instance for the given secret of descrete logarithm
         * sigma protocol.
         * @param w secret exponent value
         */
        dlog(w: bigint): ProverSecret

        /** Creates a new [[ProverSecret]] instance for the given secret of Diffie Hellman tuple
         * sigma protocol.
         * @param w secret exponent value used to compute `u = g^w` and `v = h^w`, where `g` and `h` are generators
         * @param dhtProp a [[SigmaProp]] representing public key of Diffie Hellman tuple sigma protocol, should be created using `w`
         */
        dht(w: bigint, dhtProp: SigmaProp): ProverSecret
    }

    /** Prover which can sign messages (generate proofs) for arbitrary sigma propositions
     * represented by [[SigmaProp]] values.
     *
     * See [EIP-11](https://github.com/ergoplatform/eips/pull/8) for details of multi-signature scheme.
     *
     * @see SigmaPropVerifier
     */
    export declare class SigmaPropProver {
        /**
         * A method which is generating commitments for all the public keys provided.
         * This is used as part of multi-signature scheme.
         *
         * Currently only keys in form of ProveDlog and ProveDiffieHellman are supported, not more complex subtrees.
         *
         * @param sigmaTree   - crypto-tree which is being signed
         * @param generateFor - public keys for which commitments should be generated
         * @return generated commitments in a form of prover hints
         *         - private, containing secret randomness
         *         - public, containing only commitments
         */
        generateCommitmentsFor(
            sigmaTree: SigmaProp,
            generateFor: SigmaProp[]): ProverHints

        /**
         * A method which is extracting partial proofs of secret knowledge for particular secrets with their
         * respective public images given. Useful for distributed signature applications.
         *
         * See DistributedSigSpecification for examples of usage.
         *
         * @param sigmaTree                 - public key (in form of a sigma-tree)
         * @param proof                     - signature for the key
         * @param realSecretsToExtract      - public keys of secrets with real proofs
         * @param simulatedSecretsToExtract - public keys of secrets with simulated proofs
         * @return - bag of OtherSecretProven and OtherCommitment hints
         */
        hintsForMultisig(
            sigmaTree: SigmaProp,
            proof: Int8Array,
            realSecretsToExtract: SigmaProp[],
            simulatedSecretsToExtract: SigmaProp[]): ProverHints

        /**
         * Generate commitments for given crypto-tree (sigma-tree) for prover's secrets.
         */
        generateCommitments(sigmaTree: SigmaProp): ProverHints

        /** Sign arbitrary message under a key representing a statement provable via a sigma-protocol.
         *
         * @param sigmaProp - public key
         * @param message   - message to sign
         * @param hintsBag  - additional hints for a signer (useful for distributed signing)
         * @return - signature or error
         */
        signMessage(
            sigmaProp: SigmaProp,
            message: Int8Array,
            hintsBag: ProverHints): Int8Array
    }

    export declare class SigmaPropProver$ {
        /** Creates a new [[SigmaPropProver]] with the given secrets. */
        withSecrets(secrets: ProverSecret[]): SigmaPropProver
    }

    /** Verifier which can verify signature (proof) for arbitrary sigma propositions
     * represented by [[SigmaProp]] values.
     *
     * See [EIP-11](https://github.com/ergoplatform/eips/pull/8) for details of multi-signature scheme.
     *
     * @see SigmaPropProver
     */
    export declare class SigmaPropVerifier {
        /**
         * Verify a signature on given (arbitrary) message for a given sigma proposition (public key).
         *
         * @param sigmaProp public key (represented as a sigma proposition)
         * @param message   message
         * @param signature signature for the message
         * @return whether signature is valid or not (valid signature contains proofs for the sigma proposition)
         */
        verifySignature(
            sigmaProp: SigmaProp,
            message: Int8Array,
            signature: Int8Array): boolean
    }
    export declare class SigmaPropVerifier$ {
        /** Create a new instance of [[SigmaPropVerifier]]. */
        create(): SigmaPropVerifier
    }

    export declare class Utils {
        /** Convert an Int8Array to a hex string. */
        int8ArrayToHex(arr: Int8Array): string
    }

    /** Represents a prover for signing Ergo transactions and messages.
     *
     * Equivalent of [[org.ergoplatform.sdk.SigmaProver]] available from JS.
     */
    export declare class SigmaProver {
        /** Returns the Pay-to-Public-Key (P2PK) address associated with the prover's public key.
         * The returned address corresponds to the master secret derived from the mnemonic
         * phrase configured in the [[ProverBuilder]].
         */
        getP2PKAddress(): HexString;

        /** Returns the prover's secret key. */
        getSecretKey(): bigint;

        /** Returns an array of EIP-3 addresses associated with the prover's secret keys. */
        getEip3Addresses(): HexString[];

        /** Reduces the transaction to the reduced form, which is ready to be signed.
         * @param stateCtx blockchain state context
         * @param unsignedTx unsigned transaction to be reduced (created by Fleet builders)
         * @param boxesToSpend boxes to be spent by the transaction
         * @param dataInputs data inputs to be used by the transaction
         * @param tokensToBurn tokens to be burned by the transaction
         * @param baseCost base cost of the transaction
         * @return reduced transaction
         */
        reduce(
            stateCtx: BlockchainStateContext,
            unsignedTx: UnsignedTransaction,
            boxesToSpend: EIP12UnsignedInput[],
            dataInputs: FBox<Amount, NonMandatoryRegisters>[],
            tokensToBurn: TokenAmount<Amount>[],
            baseCost: number): ReducedTransaction;

        /** Signs the reduced transaction.
         * @param reducedTx reduced transaction to be signed
         * @return signed transaction containting all the required proofs (signatures)
         */
        signReduced(reducedTx: ReducedTransaction): SignedTransaction;
    }

    /** Equivalent of [[sdk.ProverBuilder]] available from JS.
     *
     * @param parameters Blockchain parameters re-adjustable via miners voting and
     *                   voting-related data. All of them are included into extension
     *                   section of a first block of a voting epoch.
     * @param network    Network prefix to use for addresses.
     */
    export declare class ProverBuilder {
        /** Configure this builder to use the given seed when building a new prover.
         *
         * @param mnemonicPhrase          secret seed phrase to be used in prover for generating proofs.
         * @param mnemonicPass            password to protect secret seed phrase.
         */
        withMnemonic(mnemonicPhrase: HexString, mnemonicPass: HexString): ProverBuilder;

        /** Configure this builder to derive the new EIP-3 secret key with the given index.
         * The derivation uses master key derived from the mnemonic configured using
         * [[ErgoProverBuilder.withMnemonic]].
         *
         * @param index last index in the EIP-3 derivation path.
         */
        withEip3Secret(index: number): ProverBuilder;

        /** Configures this builder to use group elements (g, h, u, v) and secret x for a
         * ProveDHTuple statement when building a new prover.
         *
         * ProveDHTuple is a statement consisting of 4 group elements (g, h, u, v) and
         * requires the prover to prove knowledge of secret integer x such that.
         *
         * u = g^x
         * and
         * y = h^x
         *
         * @param g [[GroupElement]] instance defining g
         * @param h [[GroupElement]] instance defining h
         * @param u [[GroupElement]] instance defining u
         * @param v [[GroupElement]] instance defining v
         * @param x [[BigInteger]] instance defining x
         * @see
         * <a href="https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b3695bdb785c9b3a94545ffea506358ee3f8ed3d/sigmastate/src/test/scala/sigmastate/utxo/examples/DHTupleExampleSpecification.scala#L28">example</a>
         * @see
         * <a href="https://github.com/ScorexFoundation/sigmastate-interpreter/blob/b54a173865a532de09bbcbf10da32ee2a491c8f9/sigmastate/src/main/scala/sigmastate/basics/DiffieHellmanTupleProtocol.scala#L58">implementation</a>
         */
        withDHTSecret(g: HexString, h: HexString, u: HexString, v: HexString, x: bigint): ProverBuilder;

        /** This allows adding additional secret for use in proveDlog, when the secret is not
         * part of the wallet.
         *
         * Multiple secrets can be added by calling this method multiple times.
         *
         * Multiple secrets are necessary for statements that need multiple proveDlogs, such
         * as proveDlog(a) && proveDlog(b), where a and b are two group elements.
         */
        withDLogSecret(x: bigint): ProverBuilder;

        /** Builds a new prover using provided configuration. */
        build(): SigmaProver;
    }

    export declare class ProverBuilder$ {
        static create(parameters: BlockchainParameters, network: number): ProverBuilder;
    }

    /**
     * Represents a ContractTemplate parameter.
     */
    export declare class Parameter {
        /** User readable parameter name (string bytes in UTF-8 encoding) */
        name: String
        /** User readable parameter description (string bytes in UTF-8 encoding) */
        description: String
        /** Index in the ErgoTree.constants array */
        constantIndex: number
    }

    /** JavaScript class wrapping the Scala [[sigma.ast.Value]]. */
    export declare class Expr {
    }

    /**
     * Represents a reusable ContractTemplate with support to generate ErgoTree based on
     * provided parameters.
     */
    export declare class ContractTemplate {
        /**
         * The optional version of ErgoTree which should be used. If this value is not
         * provided here then it must be provided while generating the `ErgoTree` by
         * calling `applyTemplate`.
         */
        treeVersion: number | undefined;

        /** User readable name (non-empty string bytes in UTF-8 encoding). */
        name: string;

        /** User readable contract description (string bytes in UTF-8 encoding). */
        description: string;

        /** List denoting the type of ConstantPlaceholders in the expressionTree. */
        constTypes: Type[];

        /**
         * Optional list of optional default values for the ConstantPlaceholders in the
         * expressionTree. If an entry in the sequence is None, it must have a
         * corresponding entry in parameters and its value must be provided while
         * generating the `ErgoTree` by calling `applyTemplate`. If all the entries are
         * None, the whole `constValues` field can be set to None.
         */
        constValues: (Value | undefined)[] | undefined;

        /**
         * Typed template parameters of the contract template. It must have an entry for
         * each `ConstantPlaceholder` which has a `None` in the `constValues` field. Other
         * fields which do have a value defined in `constValues` can also be allowed to be
         * optionally overridden by accepting it in `parameters`.
         */
        parameters: Parameter[];

        /** Root of the contract which is a valid expression of `SigmaProp` type. Must
         * have constants segregated into `constTypes` and optionally `constValues`
         */
        expressionTree: Expr

        /** @return JSON representation of this contract template pretty-printed to a string
         *         indentation of two spaces.
         */
        toJsonString(): String

        /**
         * Generate the ErgoTree from the template by providing the values for parameters.
         *
         * @param version the version of the `ErgoTree` to use. Must be provided if the `treeVersion` was not provided in the
         * template.
         * @param paramValues the name-value map for the parameters accepted by the `ContractTemplate`. Must contain an entry
         * for each parameter for which no default value was provided in the template. Optionally, can also
         * provide values to override for parameters which do have a default value defined in the template.
         * The type of the provided value must match with the corresponding entry in the `constTypes`
         * provided in the template.
         * @return `ErgoTree` generated by replacing the template parameters with the value provided in `paramValues`.
         */
        applyTemplate(
            version: number | undefined,
            paramValues: SigmaCompilerNamedConstantsMap): ErgoTree
    }

    export declare class ContractTemplate$ {
        /** Create a new contract template from a JSON string.
         *
         * @param json JSON string representing a contract template.
         * @return a new contract template.
         */
        fromJsonString(json: String): ContractTemplate
    }
}
