"""
Implement extensions for
and Contained.... Classes.

On the base clases the only needed modifications are
the use of @acumulable decorators for methods that will be available on
Contained... Classes.

TODO: Extend documentation
"""

import logging

log = logging.getLogger(__name__)

def acumulable(fun):
    log.debug('Make {} acumulable'.format(fun.__name__))
    fun.is_acumulable=True
    return fun

def is_acumulable(fun):
    try:
        return fun.is_acumulable
    except AttributeError:
        return False

def multifun(wfun):
    def wrapper(self,*args):
        dargs = []
        for arg in args:
            try:
                if len(arg) != len(self):
                    raise ValueError('Arguments unaligned: {} != {}'.\
                                     format( len(arg),len(self) ))
                else:
                    dargs.append(arg)
            except (AttributeError,TypeError):
                dargs.append( [arg]*len(self) )

        if dargs:
            out = [wfun(s,*args) for s,args in zip(self,zip(*dargs))]
        else:
            out = [wfun(s) for s in self]

        try:
            return Container.pack(out)
        except AttributeError:
            return out

    wrapper.__name__ = wfun.__name__
    new_name = wrapper.__qualname__
    new_name = '.'.join(new_name.split('.')[:-2])
    new_name = '.'.join([new_name,'multiplied',wfun.__name__])
    wrapper.__qualname__ = new_name

    log.debug('creating {}'.format(new_name))
    log.debug('creating {}'.format(wrapper.__repr__()))

    return wrapper

class Container(object):
    contained_class = object
    def __init__(self,arglist,**kwargs):
        self.list = arglist
        try:
            self.length = Container.pack([i.length for i in arglist])
        except AttributeError:
            pass
    def __len__(self):
        return self.list.__len__()
    def __str__(self):
        return self.list.__str__()
    def __repr__(self):
        return self.list.__repr__()
    def __getitem__(self,index):
        return self.list.__getitem__(index)
    def sum(self):
        try:
            cum = self[0]
            for other in self[1:]:
                cum += other
            return cum
        except TypeError as e:
            log.warning(e)
            return NotImplemented

    @multifun
    def __add__(self,other):
        return self + other

    @multifun
    def __radd__(self,other):
        return self + other

    @classmethod
    def pack(cls,objects):
        classes_list = [o.__class__ for o in objects]
        baseclass = object
        classes = set(classes_list.pop().mro())

        # Drop not common clases
        for c in classes_list:
            classes = classes.intersection(c.mro())

        # Get closer common parent
        for c in classes:
            if issubclass(c,baseclass):
                baseclass = c

        try:
            return baseclass.Contained(objects)
        except AttributeError:
            return Container(objects)


    def __init_subclass__(cls,**kwargs):
        if 'contained_class' in kwargs:
            log.debug('{} replaces {}'.\
                        format(kwargs.get('contained_class'),cls.contained_class))
            cls.contained_class = kwargs.get('contained_class')

        log.debug('Container:decorating {}'.format(cls.__name__))
        refer_funcs = {}
        for c in cls.contained_class.mro():
            for k,v in c.__dict__.items():
                if not is_acumulable(v) and \
                   not k in cls.decorate_extra:
                    continue
                if k in refer_funcs:
                    continue
                log.debug('{}:{}'.format(k,v))
                refer_funcs[k] = v

        for name,wfun in refer_funcs.items():

            log.debug('wrapping multiple function {}'.format(wfun))

            multi = multifun(wfun)

            setattr(cls,name,multi)

"""
DelayedInt parameters

Using those for parameters instead of plain ints implies
Catching ValueErrors for unset Delayed parameters.

This is part of a brute-force reimplementation of lazyness, from the times
when i didn't knew Haskell.
"""

#from .contained import acumulable,multifun

def delayable(fun):
    def wrapper(num,*args,**kwargs):
        if num.value is None:
            #raise ValueError('PromisedInt not set yet')
            return PromisedResult(fun,num,*args)
        else:
            return fun(num.value,*args,**kwargs)

    wrapper.__name__ = fun.__name__
    new_name = wrapper.__qualname__
    new_name = '.'.join(new_name.split('.')[:-2])
    new_name = '.'.join([new_name,'delayed_wrapper',fun.__name__])
    wrapper.__qualname__ = new_name

    log.debug('creating {}'.format(new_name))

    return wrapper

class DelayedInt():
    _target_methods = {'__add__':int.__add__,
                       '__radd__':int.__radd__,
                       '__sub__':int.__sub__,
                       '__rsub__':int.__rsub__,
                       '__mul__':int.__mul__,
                       '__rmul__':int.__rmul__,
                       '__floordiv__':int.__floordiv__,
                       '__truediv__':int.__truediv__,
                       '__rfloordiv__':int.__rfloordiv__,
                       '__rtruediv__':int.__rtruediv__,
                       '__divmod__':int.__divmod__,
                       '__rdivmod__':int.__rdivmod__,
                       '__mod__':int.__mod__,
                       '__rmod__':int.__rmod__,
                       '__ge__':int.__ge__,
                       '__lt__':int.__lt__,
                       '__gt__':int.__gt__,
                       '__le__':int.__le__,
                       '__ne__':int.__ne__,
                       '__eq__':int.__eq__}

    def __init_subclass__(cls,**kwargs):
        log.debug('decorating {}'.format(cls.__name__))
        for m,f in cls._target_methods.items():
            if callable(f):
                # Decorate functions to be:
                # @acumulable
                # @delayable
                # def method....
                method = acumulable(delayable(f))
                #try:
                #    getattr(m,cls)
                #except TypeError:
                #    setattr(cls,m,method)
                setattr(cls,m,method)

class PromisedInt(DelayedInt):
    def __init__(self):
        self.value = None

    @acumulable
    def set(self, value):
        try:
            self.value = int(value)
            if abs(value) % 1 > 0:
                log.warning('PromisedInt casting float to int')
        except ValueError:
            raise

    @acumulable
    def eval(self):
        if self.value is None:
            #raise ValueError('PromisedInt not set yet')
            return self
        else:
            return self.value

    @acumulable
    def __int__(self):
        value = self.eval()
        if isinstance(value,DelayedInt):
            raise ValueError('Promise not fullfilled')
        return value

class PromisedResult(PromisedInt):
    def __init__(self,fun,*args):
        self.value = None
        self.promise = (fun,args)

    @acumulable
    def eval(self):
        fun,args = self.promise
        args = list(args)
        if hasattr(args[0],'eval'):
            args[0] = args[0].eval()
        if hasattr(args[1],'eval'):
            args[1] = args[1].eval()
        try:
            value = fun(*args)
            if not isinstance(value,DelayedInt) and \
               not value is NotImplemented:
                return value
            else:
                return self
        except TypeError as e:
            log.debug('{}'.format(e))
            return self

    @acumulable
    def __int__(self):
        value = self.eval()
        if isinstance(value,DelayedInt):
            raise ValueError('Promise not fullfilled')
        return int(value)

    @acumulable
    def __bool__(self):
        value = self.eval()
        if isinstance(value,DelayedInt):
            raise ValueError('Promise not fullfilled')
        return bool(value)


