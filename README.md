# Griffin

**Hydrophilic-Lipophilic Balance/Deviation Calculator**

A **fast, accurate, and extensible** HLB/HLD calculator written in **Common Lisp**.

<p align="center">
  <img src="assets/common-lisp.svg" width="200" />
</p>

---

## Goals

- [ ] Implement HLB & HLD calculation backend
- [ ] Implement database for surfactants & oils 
- [ ] Build unit test framework
- [ ] Build documentation system
- [ ] Build ADW/GTK4 UI
- [ ] Create executable

---

## Installation

```bash
  # Clone the repo
  $ git clone https://github.com/logoraz/griffin.git
  $ cd griffin
  # Load in your Lisp REPL (SBCL only for now)
  $ sbcl
```

```lisp
  ;; Load system
  (asdf:load-system :griffin)

  ;; Test system
  (asdf:test-system :griffin)

```

## References

- [ ] TBD
- [ ] TBD
- [ ] TBD