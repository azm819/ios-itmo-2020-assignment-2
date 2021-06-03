//
//  main.swift
//  HW2
//
//  Created by Azamat on 27.09.2020.
//

import Foundation

protocol Calculatable: Comparable, SignedNumeric {
    init?(_ string: String)
    static func / (lhs: Self, rhs: Self) -> Self
    static func % (lhs: Self, rhs: Self) -> Self
    static func ^(lhs: Self, rhs: Self) -> Self
    static func pow(_ lhs: Self, _ rhs: Self) -> Self

    static prefix func - (_ x: Self) -> Self
    static func fact(_ x: Self) -> Self
}

extension Int: Calculatable {
    static func pow(_ lhs: Int, _ rhs: Int) -> Int {
        return Int(Foundation.pow(Double(lhs), Double(rhs)))
    }

    static func fact(_ x: Int) -> Int {
        guard x > 1 else {
            return 1
        }
        return x * fact(x - 1)
    }
}

extension Double: Calculatable {
    static func % (lhs: Double, rhs: Double) -> Double {
        return Double(Int(lhs) % Int(rhs))
    }

    static func ^(lhs: Double, rhs: Double) -> Double {
        return Double(Int(lhs) ^ Int(rhs))
    }

    static func pow(_ lhs: Double, _ rhs: Double) -> Double {
        return Foundation.pow(lhs, rhs)
    }

    static func fact(_ x: Double) -> Double {
        guard x > 1 else {
            return 1
        }
        let roundedX = x.rounded()
        return roundedX * fact(roundedX - 1)
    }
}

extension Optional {
    func unwrap(or error: Error) throws -> Wrapped {
        guard let wrapped = self else {
            throw error
        }
        return wrapped
    }
}

enum Location {
    case prefix
    case postfix
}

enum Associativity {
    case none
    case left
    case right
}

enum Arity<Num: Calculatable> {
    case bracket
    case unary((Num) -> Num, Location)
    case binary((Num, Num) -> Num)
}

struct Operator<Num: Calculatable>: CustomStringConvertible {
    let value: OperatorValue
    let precedence: Precedense
    let arity: Arity<Num>
    let associativity: Associativity

    var description: String {
        return self.value.rawValue
    }
}

enum Token<Num: Calculatable>: CustomStringConvertible {
    case value(Num)
    case `operator`(Operator<Num>)
    case leftBracket
    case rightBracket

    var description: String {
        switch self {
        case .value(let num): return "\(num)"
        case .operator(let op): return op.description
        case .leftBracket: return "("
        case .rightBracket: return ")"
        }
    }
}

enum OperatorValue: String {
    case plus = "+"
    case minus = "-"
    case multiplication = "*"
    case division = "/"
    case mod = "%"
    case power = "^"
    case factorial = "!"
    case leftBracket = "("
}

enum Precedense: Int {
    case none = 0
    case low
    case medium
    case high
}

func defaultBinaryOperators<Num: Calculatable>() -> [Operator<Num>] {
    [
        Operator(value: .plus, precedence: .low, arity: .binary(+), associativity: .left),
        Operator(value: .minus, precedence: .low, arity: .binary(-), associativity: .left),
        Operator(value: .multiplication, precedence: .medium, arity: .binary(*), associativity: .left),
    ]
}

func additionalBinaryOperators<Num: Calculatable>() -> [Operator<Num>] {
    [
        Operator(value: .division, precedence: .medium, arity: .binary(/), associativity: .left),
        Operator(value: .mod, precedence: .medium, arity: .binary(%), associativity: .left),
        Operator(value: .power, precedence: .medium, arity: .binary(Num.pow), associativity: .right),
    ]
}

func additionalUnaryOperators<Num: Calculatable>() -> [Operator<Num>] {
    let res = [
        Operator(value: .factorial, precedence: .high, arity: .unary(Num.fact, .postfix), associativity: .right)
    ]

    switch mode {
    case .infix:
        return res + [
            Operator(value: .minus, precedence: .high, arity: .unary(-, .prefix), associativity: .right),
            Operator(value: .plus, precedence: .high, arity: .unary(abs, .prefix), associativity: .right)
        ]
    case .postfix:
        return res
    }
}

enum EvaluationError: Error, CustomStringConvertible {
    case invalidToken(token: String)
    case arityError
    case bracketSequenceError
    case testingError(mode: Mode, testNumber: Int)

    var description: String {
        switch self {
        case .invalidToken(token: let token):
            return "Invalid token: \"\(token)\""
        case .arityError:
            return "Arity error"
        case .bracketSequenceError:
            return "Check bracket sequence"
        case .testingError(mode: let mode, testNumber: let testNumber):
            return "Test number \(testNumber) failed in \(mode) mode testing"
        }
    }
}

func evalInRpn<Num: Calculatable>(tokens: [Token<Num>]) throws -> Num {
    let valStack: [Num] = try tokens.reduce(into: [Num]()) { (valStack, token) in
        switch token {
        case .value(let num):
            valStack.append(num)
        case .operator(let op):
            switch op.arity {
            case .unary(let f, _):
                guard let element = valStack.popLast() else {
                    throw EvaluationError.arityError
                }
                valStack.append(f(element))
            case .binary(let f):
                guard let rhs = valStack.popLast(), let lhs = valStack.popLast() else {
                    throw EvaluationError.arityError
                }
                valStack.append(f(lhs, rhs))
            default:
                throw EvaluationError.invalidToken(token: token.description)
            }
        case .leftBracket:
            throw EvaluationError.invalidToken(token: token.description)
        case .rightBracket:
            throw EvaluationError.invalidToken(token: token.description)
        }
    }

    guard let result = valStack.first, valStack.count == 1 else {
        throw EvaluationError.arityError
    }

    return result
}

func eval<Num: Calculatable>(_ input: [String], binaryOperators binOps: [Operator<Num>] = defaultBinaryOperators(), unaryOperators unOps: [Operator<Num>] = []) throws -> Num {
    let binaryOperators: [String: Operator<Num>] = Dictionary(uniqueKeysWithValues: binOps.map { ($0.value.rawValue, $0) })
    let unaryOperators: [String: Operator<Num>] = Dictionary(uniqueKeysWithValues: unOps.map { ($0.value.rawValue, $0) })

    let isInfixMode = mode == .infix
    var shouldBeUnary = isInfixMode
    var bracketsBalance = 0
    let tokens: [Token<Num>] = try input.map {
        let token: Token<Num>?
        if let t = Num($0).map(Token.value) {
            token = t
            shouldBeUnary = false
        } else if $0 == "(" {
            guard mode == .infix else {
                throw EvaluationError.invalidToken(token: "postfix notation must not contain brackets")
            }
            bracketsBalance += 1
            token = .leftBracket
            shouldBeUnary = isInfixMode
        } else if $0 == ")" {
            guard mode == .infix else {
                throw EvaluationError.invalidToken(token: "postfix notation must not contain brackets")
            }
            guard bracketsBalance > 0 else {
                throw EvaluationError.bracketSequenceError
            }
            bracketsBalance -= 1
            token = .rightBracket
            shouldBeUnary = false
        } else {
            let op = (shouldBeUnary ? unaryOperators[$0] : binaryOperators[$0]) ?? unaryOperators[$0]
            switch op?.arity {
            case .none:
                shouldBeUnary = isInfixMode
            case .unary(_, let location):
                shouldBeUnary = location == .prefix
            case .some(.bracket):
                shouldBeUnary = isInfixMode
            case .some(.binary(_)):
                shouldBeUnary = isInfixMode
            }
            token = op.map(Token.operator)
        }
        return try token.unwrap(or: EvaluationError.invalidToken(token: $0))
    }
    guard bracketsBalance == .zero else {
        throw EvaluationError.bracketSequenceError
    }

    switch mode {
    case .infix:
        let rpnExt: (rpn: [Token<Num>], opStack: [Operator<Num>]) = tokens.reduce(into: (rpn: [], opStack: [])) { (acc, token) in
            switch token {
            case .value:
                acc.rpn.append(token)
            case .operator(let op):
                while let topOp = acc.opStack.last, op.associativity == .left && topOp.precedence.rawValue >= op.precedence.rawValue ||
                    op.associativity == .right && topOp.precedence.rawValue > op.precedence.rawValue {
                        acc.rpn.append(.operator(topOp))
                        acc.opStack.removeLast()
                }
                acc.opStack.append(op)
            case .leftBracket:
                acc.opStack.append(Operator(value: .leftBracket, precedence: .none, arity: .bracket, associativity: .none))
            case .rightBracket:
                while let op = acc.opStack.last, op.value != .leftBracket {
                    acc.rpn.append(.operator(op))
                    acc.opStack.removeLast()
                }
                acc.opStack.removeLast() /// remove left bracket from ops
            }
        }

        let rpn = rpnExt.rpn + rpnExt.opStack.reversed().map(Token.operator)

        return try evalInRpn(tokens: rpn)
    case .postfix:
        return try evalInRpn(tokens: tokens)
    }
}


enum Mode {
    case infix
    case postfix
}

var mode: Mode = .infix

func testInternal<Num: Calculatable>(forMode testMode: Mode, withTests testStrings: [String: Num]) {
    print("*** testing \(testMode) mode ***")
    mode = testMode
    let binaryOperators: [Operator<Num>] = defaultBinaryOperators() + additionalBinaryOperators()
    let unaryOperators: [Operator<Num>] = additionalUnaryOperators()

    for (index, test) in testStrings.enumerated() {
        do {
            let res = try eval(test.key.components(separatedBy: .whitespaces), binaryOperators: binaryOperators, unaryOperators: unaryOperators)

            guard res == test.value else {
                throw EvaluationError.testingError(mode: mode, testNumber: index + 1)
            }
        } catch {
            print(error)
        }
    }

    print("*** \(testMode) mode testing completed ***")
}

func test() {
    testInternal(forMode: .infix, withTests: ["2 ^ 3": 8,
                                              "2 ^ 3 ^ 3": 134217728,
                                              "( 2 ^ 3 ) ^ 3": 512])
    testInternal(forMode: .postfix, withTests: ["2 2 +": 4,
                                                "2 3 - 5 * 6 + 2 2 3 ^ ^ -": -255])
}

func launch() {
    print("Enter expression")
    var components = CommandLine.arguments
    switch components.first {
    case "--infix":
        mode = .infix
        components.removeFirst()
    case "--postfix":
        mode = .postfix
        components.removeFirst()
    case "--test":
        test()
        return
    default:
        break
    }
    do {
        print(try eval(components, binaryOperators: defaultBinaryOperators() + additionalBinaryOperators(), unaryOperators: additionalUnaryOperators()) as Double)
    } catch {
        print(error)
    }
}

launch()
